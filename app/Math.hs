module Math where

import Types (InputValue(..))
import Data.Char (isAlpha, isDigit)
import Text.Read (readMaybe)

handleInput :: String -> IO InputValue
handleInput input =
  case words input of
    [s]
      | all isAlpha s || (isAlpha (head s) && all isDigit (tail s)) -> return (S s)
      | Just i <- readMaybe s -> return (I i)

    [s, numStr]
      | Just i <- readMaybe numStr, i >= 0, i <= 100 -> return (SI s i)

    [s1, s2] -> return $ SS s1 s2

    [i1, i2, i3]
      | Just r <- readMaybe i1
      , Just g <- readMaybe i2
      , Just b <- readMaybe i3 -> return $ III r g b

    _ -> return (S "")  -- default case, returns an empty string in case of invalid input

getListIndex :: [a] -> [Int]
getListIndex xs = [i | (i, _) <- zip [0..] xs]

getTrueIndexes :: [Bool] -> [Int]
getTrueIndexes boolList = [i | (i, True) <- zip [0..] boolList]

convertRgbToXy :: (Int, Int, Int) -> (Double, Double) -- CIE 1931 light color supported by Hue Bridge API
convertRgbToXy (r, g, b) = (xHat, yHat)
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b
    x      = 0.4124 * r' + 0.3576 * g' + 0.1805 * b'
    y      = 0.2126 * r' + 0.7152 * g' + 0.0722 * b'
    z      = 0.0193 * r' + 0.1192 * g' + 0.9505 * b'
    total  = x + y + z
    (xHat, yHat)
      | total == 0 = (0,0)
      | otherwise  = (x / total, y / total)