import System.IO
import System.Directory
import Codec.Picture
import Data.Numbers.Primes

-- interval

ns :: [Int]
ns = [0..7] 

-- png dimensions

w :: Int
w = 107 --2560

h :: Int
h = 107 --1440

-- stdout colors

data Color = Reset | R | G | Y | B | M | C | W

code :: Color -> Int
code Reset = 0
code R = 31
code G = 32
code Y = 33
code B = 34
code M = 35
code C = 36
code W = 37

instance Show Color where
    show c = "\x1b[" ++ (show . code) c ++ "m"

type ColorOut = Color -> String -> IO () 

putClrLn :: ColorOut
putClrLn c s = putStrLn $ show c ++ s ++ show Reset

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