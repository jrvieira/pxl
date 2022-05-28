import System.Environment
import System.Directory
import Data.Numbers.Primes
import Color
import Codec.Picture

main :: IO ()
main = do
   createDirectoryIfMissing True "io"
   args <- getArgs
   go args
   where
   go args
      | [w,h,from,to] <- read <$> args = mapM_ (draw (w,h)) [from .. to]
      | otherwise = putClr R "Exactly 4 arguments needed:" >> putClrLn W "width height from to"

-- rendering

type Dimensions = (Int,Int)

draw :: Dimensions -> Int -> IO ()
draw (w,h) n = do
   savePngImage file $ ImageY8 $ generateImage (xtypz n) w h
   putClrLn G file -- print drawn file to console
   where
   file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_xtyp" ++ show n ++ ".png"

-- function

xtypz :: Int -> Int -> Int -> Pixel8
xtypz z x y = if isPrime (x*y+z) then 0 else 255

