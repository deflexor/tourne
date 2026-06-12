{-|
Module      : Tourne.TUI.Events.Util
Description : Small helpers shared between the event-handler submodules.

'clamp'    -- clamp an @Int@ into a closed range
'isPrint'  -- is this a printable ASCII character?
'modifySt' -- @modify'@ in @EventM@ (Relude doesn't export it)

These are tiny; the goal is to share them between the per-mode
event submodules without each redefining its own copy.
-}
module Tourne.TUI.Events.Util
  ( clamp
  , isPrint
  , modifySt
  ) where

import Relude
import Brick.Types (EventM)

-- | Clamp @x@ to the inclusive range @[lo, hi]@.
clamp :: Int -> Int -> Int -> Int
clamp lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

-- | Is this character printable (ASCII 0x20..0x7E)?
isPrint :: Char -> Bool
isPrint c = c >= ' ' && c <= '~'

-- | State modification helper (Relude doesn't export modify').
modifySt :: MonadState s m => (s -> s) -> m ()
modifySt f = get >>= put . f
