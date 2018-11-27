import Color
import System.IO
import qualified Data.Map as Map
import Data.Numbers.Primes
import Codec.Picture
import System.Directory

-- interval

ns :: [Int]
ns = [0..13] 

-- png dimensions

w :: Int
w = 2560 --2560

h :: Int
h = 1440 --1440

-- pixels

black :: Pixel8
black = 0

white :: Pixel8
white = 255

-- function

xtypz :: Int -> Int -> Int -> Pixel8
xtypz z x y = if isPrime (x*y+z) then black else white

-- rendering

draw :: Int -> IO ()
draw n = do
    let file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_xtyp" ++ show n ++ ".png"
    done <- doesFileExist file
    if done
        then putClrLn W file
        else do
            savePngImage file (ImageY8 (generateImage (xtypz n) w h))
            putClrLn G file

-- -- draw many xtyp [z]

draw_list :: [Int] -> IO ()
draw_list list = do
    mapM_ draw list

-- prompt

main :: IO ()
main = do
    createDirectoryIfMissing True "io"
    draw_list ns