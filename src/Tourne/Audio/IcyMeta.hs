{-|
Module      : Tourne.Audio.IcyMeta
Description : Pure parsers for SHOUTcast / Icecast ICY metadata.

ICY (in-band metadata over the same HTTP connection as the audio
stream) is a venerable Shoutcast convention. The client opts in by
sending @Icy-MetaData: 1@ in the GET request. The server responds
with an @icy-metaint: N@ header, and then inserts a metadata block
every @N@ bytes of audio. A block is one length byte @L@ followed by
@L * 16@ bytes of ASCII metadata, typically shaped like:

> StreamTitle='Artist - Title';StreamUrl='https://...';

The functions in this module are pure and do no IO. They are used by
'Tourne.Audio.Stream' to walk the byte stream and by
'Tourne.Audio.Player' to publish parsed titles to the TUI.

Three concerns, three functions:

* 'parseIcyMetaint'   — read the @icy-metaint@ response header.
* 'stripIcyMeta'      — walk a chunk, separating audio bytes from
                        metadata bytes. Stateful: the caller threads
                        the running position through consecutive
                        chunks.
* 'parseStreamTitle'  — extract the @StreamTitle=...;@ value from a
                        metadata block.
-}
module Tourne.Audio.IcyMeta
  ( -- * Headers
    parseIcyMetaint
    -- * Byte stream
  , stripIcyMeta
  , IcyState(..)
  , initialIcyState
    -- * Metadata body
  , parseStreamTitle
  ) where

import Relude
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace, toLower)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.Header (Header, HeaderName)

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

-- | Read the @icy-metaint@ response header.
--
-- Returns @Just n@ where @n > 0@ if the server sent a valid positive
-- integer in the header, otherwise 'Nothing'. The @case-insensitive@
-- package was deliberately not reintroduced after the Streamly
-- refactor (@f38fcf7@). We compare the @show@-rendered
-- @'HeaderName'@ (which is a @CI ByteString@) against the literal
-- form: @show h@ on a CI value renders the underlying bytes wrapped
-- in quotes, so we strip the quotes before comparing.
parseIcyMetaint :: [Header] -> Maybe Int
parseIcyMetaint headers =
  foldr go Nothing headers
  where
    go (name, value) acc
      | stripQuotes (show name) == "icy-metaint"
      , Just (n, rest) <- BSC.readInt (BSC.dropWhile isSpace value)
      , BS.null (BSC.dropWhile isSpace rest)
      , n > 0
      = Just n
      | otherwise = acc

    -- 'show' on a 'HeaderName' (which is a CI newtype) renders the
    -- underlying bytes wrapped in double quotes: @"\"icy-metaint\""@.
    -- Drop the first and last characters if both are double quotes.
    stripQuotes :: String -> String
    stripQuotes ('"':xs) = drop1 (reverse xs)
      where
        drop1 ('"':ys) = reverse ys
        drop1 _        = '"':xs
    stripQuotes s = s

--------------------------------------------------------------------------------
-- Byte stream
--------------------------------------------------------------------------------

-- | The position-tracking state threaded through 'stripIcyMeta'
-- across consecutive chunks.
--
-- Semantics of @icyRemaining@:
--
--   * @> 0@ — we are still inside a run of clean audio; this many
--     more bytes of audio may be emitted before the next metadata
--     boundary is reached.
--   * @== 0@ — we are exactly at a metadata boundary; the next byte
--     is the length header for the upcoming metadata block.
--   * @< 0@ — we are inside a metadata body that was truncated by a
--     chunk boundary; the magnitude is the number of metadata bytes
--     still to skip before audio resumes.
data IcyState = IcyState
  { icyInterval  :: !Int
  , icyRemaining :: !Int
  } deriving (Eq, Show)

-- | Initial state for a stream with the given ICY metadata interval
-- (0 means no ICY metadata). When the interval is 0, 'stripIcyMeta'
-- short-circuits and returns input unchanged.
initialIcyState :: Int -> IcyState
initialIcyState 0         = IcyState 0 0
initialIcyState interval  = IcyState interval interval

-- | Walk a chunk of bytes, separating audio from metadata.
--
-- Returns the clean audio bytes that may be fed to the MP3 decoder,
-- the body of any metadata block that was just completed in this
-- chunk (so the caller can parse 'StreamTitle=' out of it), and the
-- new state. The state must be threaded through consecutive chunks
-- so that the algorithm can resume across chunk boundaries.
--
-- When the interval is 0 (no ICY metadata advertised), the input is
-- returned unchanged and no metadata block is reported.
--
-- The 'Maybe' in the result is @Just body@ if and only if a complete
-- metadata block (length byte + L*16 body bytes) was consumed in
-- this call. Truncated bodies (split across chunks) don't fire
-- the callback — only complete blocks do.
--
-- The algorithm:
--
--   1. While we have audio to consume, copy bytes into the output
--      until either the chunk is exhausted or we hit a metadata
--      boundary.
--   2. At a boundary, consume one length byte @L@ and then @L*16@
--      bytes of metadata body. If the body is truncated by the end
--      of the chunk, record the deficit as a negative remaining and
--      wait for the next chunk to finish skipping the body.
stripIcyMeta
  :: IcyState
  -> ByteString
  -> (ByteString, Maybe ByteString, IcyState)
stripIcyMeta st bs
  | icyInterval st <= 0 = (bs, Nothing, st)
  | otherwise = go mempty Nothing (icyRemaining st) bs
  where
    interval = icyInterval st

    -- The accumulator `acc` is extended only in the `n > 0` branch
    -- (when we're emitting clean audio). The `n == 0` and
    -- `n < 0` branches pass it through unchanged, since metadata
    -- blocks never produce output.
    --
    -- The `mAcc` accumulator is for the metadata body bytes; it's
    -- only extended when we're inside a metadata body (n == 0 and
    -- the body is being consumed, or n < 0 across chunks).
    go acc mAcc n bytes
      | BS.null bytes =
          ( BS.concat (reverse acc)
          , mAcc
          , st { icyRemaining = n }
          )
      | n > 0 =
          let takeLen      = min n (BS.length bytes)
              (clean, rest) = BS.splitAt takeLen bytes
              acc'         = clean : acc
              n'           = n - takeLen
          in go acc' mAcc n' rest

      | n == 0 =
          -- At metadata boundary: 1 length byte, then L*16 body bytes.
          case BS.uncons bytes of
            Nothing ->
              -- Chunk ended exactly on the boundary; nothing more to
              -- do. Next chunk's first byte will be the length header.
              (BS.concat (reverse acc), Nothing, st { icyRemaining = 0 })
            Just (metaLenByte, afterLen) ->
              let metaLen       = fromIntegral metaLenByte * 16
                  availableMeta = BS.length afterLen
              in if availableMeta >= metaLen
                 then
                   -- Whole metadata block present in this chunk; skip
                   -- it and resume audio collection from the next byte.
                   -- Report the body we just consumed.
                   let body       = BS.take metaLen afterLen
                       afterBody = BS.drop metaLen afterLen
                   in go acc (Just body) interval afterBody
                 else
                   -- Metadata body truncated by chunk boundary; skip
                   -- what we have and remember the deficit as a
                   -- negative remaining so the next chunk continues
                   -- skipping.
                   let deficit = metaLen - availableMeta
                   in ( BS.concat (reverse acc)
                      , Nothing
                      , st { icyRemaining = -deficit }
                      )

      | otherwise =
          -- Negative n: still skipping a metadata body that started
          -- in a previous chunk. -n is the bytes still to skip.
          let skip = min (-n) (BS.length bytes)
              rest = BS.drop skip bytes
          in if skip == (-n)
             then
               -- Finished skipping. mAcc carries the body we built
               -- up across chunks. The caller wants the body of a
               -- JUST-completed block, which is exactly what mAcc
               -- holds at this point.
               go acc mAcc interval rest
             else go acc mAcc (n + skip) rest

--------------------------------------------------------------------------------
-- Metadata body
--------------------------------------------------------------------------------

-- | Parse a metadata block to extract @StreamTitle=...@.
--
-- Icecast / Shoutcast metadata blocks look like (single quotes, with
-- embedded @;@ delimiters):
--
-- > StreamTitle='Artist - Title';StreamUrl='https://example.com/';
--
-- Returns 'Nothing' if the input is empty, has no @StreamTitle=@, or
-- only carries a @StreamUrl=@. Single quotes are stripped from the
-- value (both leading-only and leading+trailing cases are handled).
-- Surrounding whitespace is trimmed.
parseStreamTitle :: ByteString -> Maybe Text
parseStreamTitle bs
  | BS.null bs = Nothing
  | otherwise = case findTitle (BS.split semi bs) of
      Just raw -> Just (Text.strip (TE.decodeUtf8 raw))
      Nothing  -> Nothing
  where
    semi :: Word8
    semi = 0x3b  -- ';'

    -- Walk the semicolon-separated fields; return the first
    -- StreamTitle value. Empty values are allowed (the server may
    -- advertise 'StreamTitle=;' as a "now playing nothing" signal).
    findTitle :: [ByteString] -> Maybe ByteString
    findTitle []     = Nothing
    findTitle (x:xs)
      | Just v <- stripTitlePrefix x
      = Just v
      | otherwise = findTitle xs

    -- Compare the first 12 bytes case-insensitively to "StreamTitle=".
    -- Returns the rest of the haystack with surrounding single
    -- quotes removed, or Nothing if the prefix doesn't match.
    stripTitlePrefix :: ByteString -> Maybe ByteString
    stripTitlePrefix haystack
      | BS.length haystack < 12 = Nothing
      | map toLower (BSC.unpack (BSC.take 12 haystack))
          == map toLower (BSC.unpack "StreamTitle=")
      = Just (stripQuotes (BS.drop 12 haystack))
      | otherwise = Nothing

    -- Strip a pair of surrounding ASCII single quotes (0x27). If
    -- only a leading quote is present (the trailing one was
    -- truncated by a stray @;@ inside the value, a known ICY edge
    -- case), strip just the leading quote. If no quotes, return
    -- unchanged.
    stripQuotes :: ByteString -> ByteString
    stripQuotes v
      | BS.length v >= 2
      , BS.head v == 0x27
      , BS.last v == 0x27
      = BS.init (BS.tail v)
      | BS.length v >= 1
      , BS.head v == 0x27
      = BS.tail v
      | otherwise = v
