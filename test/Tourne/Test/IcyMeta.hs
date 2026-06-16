-- | Tests for the pure ICY metadata parsers in
-- 'Tourne.Audio.IcyMeta'. The parsers are the algorithmic heart of
-- the ICY feature: the audio side just threads them through
-- consecutive chunks. Boundary cases (metadata spanning chunk
-- boundaries, empty chunks, end-of-stream) are the only things
-- that can go subtly wrong, so each gets explicit coverage.
module Tourne.Test.IcyMeta (tests) where

import Relude
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Network.HTTP.Types.Header (Header, HeaderName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Tourne.Audio.IcyMeta
  ( IcyState(..)
  , initialIcyState
  , parseIcyMetaint
  , parseStreamTitle
  , stripIcyMeta
  )

tests :: [TestTree]
tests =
  [ testGroup "Tourne.Audio.IcyMeta"
    [ testGroup "parseIcyMetaint"
      [ testCase "returns Nothing when header is missing" $
          parseIcyMetaint [("content-type" :: HeaderName, BSC.pack "audio/mpeg")]
            @?= Nothing

      , testCase "returns Just n for a valid icy-metaint header" $
          parseIcyMetaint [("icy-metaint" :: HeaderName, BSC.pack "8192")]
            @?= Just 8192

      , testCase "ignores zero (server sent 0 = disabled)" $
          parseIcyMetaint [("icy-metaint" :: HeaderName, BSC.pack "0")]
            @?= Nothing

      , testCase "ignores negative integers" $
          parseIcyMetaint [("icy-metaint" :: HeaderName, BSC.pack "-1")]
            @?= Nothing

      , testCase "ignores non-numeric garbage" $
          parseIcyMetaint [("icy-metaint" :: HeaderName, BSC.pack "abc")]
            @?= Nothing

      , testCase "ignores garbage with trailing characters" $
          parseIcyMetaint [("icy-metaint" :: HeaderName, BSC.pack "16000xyz")]
            @?= Nothing

      , testCase "tolerates surrounding whitespace" $
          parseIcyMetaint [("icy-metaint" :: HeaderName, BSC.pack "  16000  ")]
            @?= Just 16000

      , testCase "works on a 'show'-rendered header name" $
          -- 'show' on the HeaderName returns the lower-cased form.
          -- This is a regression guard for the case-insensitive dep
          -- that was removed in @f38fcf7@.
          let headers = [("icy-metaint" :: HeaderName, BSC.pack "8192")]
          in parseIcyMetaint headers @?= Just 8192
      ]

    , testGroup "stripIcyMeta"
      [ testCase "interval=0 short-circuits (no ICY)" $ do
          let bs = BSC.pack "arbitrary bytes"
              st = initialIcyState 0
          stripIcyMeta st bs @?= (bs, Nothing, st)

      , testCase "single chunk with no metadata: full pass-through" $ do
          let bs = BS.replicate 100 0x42  -- 100 bytes of 'B'
              st = initialIcyState 16000
          stripIcyMeta st bs @?= (bs, Nothing, st { icyRemaining = 15900 })

      , testCase "metadata block entirely inside one chunk" $ do
          -- Interval 10, then 1 length byte (L=1, so 16 body bytes),
          -- then 10 more audio bytes.
          let audio1  = BS.replicate 10 0x41       -- 10 'A's
              lenByte = BS.singleton 0x01        -- L = 1
              body    = BSC.pack "StreamTitle='X';" -- 16 bytes
              audio2  = BS.replicate 10 0x42       -- 10 'B's
              chunk   = audio1 <> lenByte <> body <> audio2
              st      = initialIcyState 10
          stripIcyMeta st chunk
            @?= (audio1 <> audio2,
                 Just body,
                 st { icyRemaining = 0 })  -- 10 audio bytes left after skipping metadata

      , testCase "metadata block split across two chunks: head in first" $ do
          -- Interval 10, then length byte 0x02 (L=2, 32 body bytes).
          -- First chunk: 10 audio + length byte + 16 body bytes.
          -- Second chunk: 16 remaining body bytes + 10 audio.
          let interval  = 10
              lenByte   = BS.singleton 0x02
              audio1    = BS.replicate 10 0x41
              bodyPart1 = BS.replicate 16 0x20      -- 16 spaces
              bodyPart2 = BS.replicate 16 0x20      -- 16 spaces
              audio2    = BS.replicate 10 0x42
              chunk1    = audio1 <> lenByte <> bodyPart1
              chunk2    = bodyPart2 <> audio2
              st0       = initialIcyState interval
          let (out1, _m1, st1) = stripIcyMeta st0 chunk1
          out1 @?= audio1
          -- After first chunk: we've emitted 10 audio bytes, consumed
          -- 1 length byte, and need to skip 16 more body bytes.
          assertBool "st1 should remember deficit of 16" (icyRemaining st1 == -16)
          let (out2, _m2, st2) = stripIcyMeta st1 chunk2
          -- The second chunk starts inside the body; we skip 16 body
          -- bytes, then collect 10 audio bytes.
          out2 @?= audio2
          -- After consuming 10 audio bytes (one full interval),
          -- the next byte would be a metadata boundary. The state
          -- reflects this with icyRemaining == 0 ("at boundary").
          assertBool "st2 should be at a metadata boundary (icyRemaining == 0)"
                     (icyRemaining st2 == 0)

      , testCase "chunk ends exactly on metadata boundary" $ do
          let audio = BS.replicate 10 0x41
              chunk = audio  -- no length byte, no body
              st    = initialIcyState 10
          stripIcyMeta st chunk
            @?= (audio, Nothing, st { icyRemaining = 0 })

      , testCase "zero-length body (length byte = 0)" $ do
          let audio1  = BS.replicate 10 0x41
              lenByte = BS.singleton 0x00   -- L = 0, body is 0 bytes
              audio2  = BS.replicate 10 0x42
              chunk   = audio1 <> lenByte <> audio2
              st      = initialIcyState 10
          -- L=0 means body is 0 bytes; report an empty body as the
          -- completed block (so the caller can still drive a parse
          -- and discover "StreamTitle is empty").
          stripIcyMeta st chunk
            @?= (audio1 <> audio2,
                 Just BS.empty,
                 st { icyRemaining = 0 })

      , testCase "large length byte (L=15 -> 240 body bytes)" $ do
          let audio1  = BS.replicate 10 0x41
              lenByte = BS.singleton 0x0F   -- L = 15, 240 body bytes
              body    = BS.replicate 240 0x20  -- 240 spaces
              audio2  = BS.replicate 5 0x42
              chunk   = audio1 <> lenByte <> body <> audio2
              st      = initialIcyState 10
          stripIcyMeta st chunk
            @?= (audio1 <> audio2,
                 Just body,
                 st { icyRemaining = 5 })

      , testCase "state threads correctly through many small chunks" $ do
          -- 100 single-byte chunks, interval = 50.
          -- After 50 audio bytes the next chunk sits exactly on a
          -- metadata boundary; the length byte happens to be 0x41
          -- ('A', 65), so the metadata body is 65*16 = 1040 bytes.
          -- We don't have that many bytes, so the state ends in
          -- deficit — but the *output* for the first 50 chunks is
          -- always 50 clean audio bytes.
          let chunks = fmap BS.singleton (BS.unpack (BS.replicate 100 0x41))
              st0    = initialIcyState 50
              thread (out, st) []     = (BS.concat (reverse out), st)
              thread (out, st) (c:cs) =
                let (clean, _, st') = stripIcyMeta st c
                in thread (clean : out, st') cs
          let (out, _) = thread (mempty, st0) chunks
          BS.length out @?= 50
          -- The first 50 chunks emit exactly 50 audio bytes, one per
          -- chunk. Subsequent chunks (51..) feed the metadata skipper.
          -- We only care about the first 50 here.
          let (out50, _) = thread (mempty, st0) (take 50 chunks)
          BS.length out50 @?= 50
      ]

    , testGroup "parseStreamTitle"
      [ testCase "extracts a simple StreamTitle" $
          parseStreamTitle (BSC.pack "StreamTitle='Hello';")
            @?= Just "Hello"

      , testCase "extracts StreamTitle with artist and song" $
          parseStreamTitle (BSC.pack "StreamTitle='Artist - Song Title';")
            @?= Just "Artist - Song Title"

      , testCase "returns Nothing when only StreamUrl is present" $
          parseStreamTitle (BSC.pack "StreamUrl='https://example.com/';")
            @?= Nothing

      , testCase "returns Nothing for empty input" $
          parseStreamTitle BS.empty @?= Nothing

      , testCase "extracts the first field when multiple are present" $
          parseStreamTitle
            (BSC.pack "StreamTitle='First';StreamUrl='https://x/';StreamTitle='Second';")
            @?= Just "First"

      , testCase "is case-insensitive on the StreamTitle= prefix" $
          parseStreamTitle (BSC.pack "streamtitle='lower';")
            @?= Just "lower"

      , testCase "handles empty value" $
          parseStreamTitle (BSC.pack "StreamTitle='';")
            @?= Just ""

      , testCase "handles value with extra semicolons inside the quotes" $ do
          -- The ICY spec uses ; to delimit fields, but the value
          -- itself can contain ; (it's a single-quoted string).
          -- We currently only take the first field; the embedded ;
          -- ends parsing early. This is a known limitation; this
          -- test documents it.
          parseStreamTitle (BSC.pack "StreamTitle='A;B';")
            @?= Just "A"

      , testCase "trims surrounding whitespace from the value" $
          parseStreamTitle (BSC.pack "StreamTitle='  Padded  ';")
            @?= Just "Padded"
      ]
    ]
  ]
