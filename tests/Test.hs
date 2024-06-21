module Main where

import Control.Monad
import Data.List
import SeeReason.GUID
import System.Exit

main = do
  as <- test 1
  bs <- test 2
  assert "(as == (nub . sort) as)" (as == (nub . sort) as)
  assert "(bs == (nub . sort) bs)" (bs == (nub . sort) bs)
  assert "(sort (as <> bs) == (nub . sort) (as <> bs))" (sort (as <> bs) == (nub . sort) (as <> bs))
  assert "(let abs = as <> bs in sort abs == (nub . sort . reverse $ abs))" (let abs = as <> bs in sort abs == (nub . sort . reverse $ abs))
  exitWith ExitSuccess

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p $ do
  error $ "assertion failed : " <> msg


test :: GUIDNode -> IO [GUID]
test node = do
  let n = 3
  gen <- newGUIDGenerator defaultGUIDConfig node
  sequence $ take n $ repeat (nextGUID gen)
