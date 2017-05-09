-- CLaSH Tutorial
-- http://hackage.haskell.org/package/clash-prelude-0.11.2/docs/CLaSH-Tutorial.html

module MAC where

import CLaSH.Prelude

ma :: Num a => a -> (a, a) -> a
ma acc (x, y) = acc + x * y

-- A principled way to describe a sequential circuit is to use one of the classic machine models
macT :: Num a => a -> (a, a) -> (a, a)
macT state input = (newstate, output)
  where
    newstate = ma state input
    output = state

-- Create a synchronous function from a combinational function describing a mealy machine
-- mealy :: (s -> i -> (s, o)) -> s -> Signal i -> Signal o
mac :: Num a => Signal (a, a) -> Signal a
mac = mealy macT 0

topEntity :: Signal (Signed 9, Signed 9) -> Signal (Signed 9)
topEntity = mac

testInput :: Signal (Signed 9, Signed 9)
testInput = stimuliGenerator $(listToVecTH ([(1,1), (2,2), (3,3), (4,4)] :: [(Signed 9, Signed 9)]))

expectedOutput :: Signal (Signed 9) -> Signal Bool
expectedOutput = outputVerifier $(listToVecTH ([0, 9, 5, 14] :: [Signed 9]))
