module Main (main) where

import Relude
import Test.Tasty (defaultMain, testGroup)
import qualified Tourne.Test.Sorting as Sorting
import qualified Tourne.Test.Persistence as Persistence
import qualified Tourne.Test.Mpg123 as Mpg123
import qualified Tourne.Test.Error as Error
import qualified Tourne.Test.SafeExceptions as SafeExceptions
import qualified Tourne.Test.HttpClient as HttpClient
import qualified Tourne.Test.Tracer as Tracer
import qualified Tourne.Test.Util as Util

main :: IO ()
main = defaultMain $ testGroup "Tourne"
  [ testGroup "Tourne.Types.sortStations" Sorting.tests
  , testGroup "Tourne.Persistence" Persistence.tests
  , testGroup "Tourne.Audio.Types.Mpg123" Mpg123.tests
  , testGroup "Tourne.Error" Error.tests
  , testGroup "Tourne.SafeExceptions" SafeExceptions.tests
  , testGroup "Tourne.Effect.HttpClient" HttpClient.tests
  , testGroup "Tourne.Effect.Tracer" Tracer.tests
  , testGroup "Tourne.Util" Util.tests
  ]
