import System.Environment
import System.Directory
import Data.Numbers.Primes
import Color
import Codec.Picture

-- prompt

main :: IO ()
main = do
   createDirectoryIfMissing True "io"
   args <- getArgs
   if length args == 4 then do
      go args
   else do
      putClr R "Args:"
      putClrLn W "width height from to"
      pure ()

go :: [String] -> IO ()
go args = mapM_ (draw (w,h)) [from .. to]
   where
   (w:h:from:to:_) = map read args

-- dimensions

type Dimensions = (Int,Int)

-- function

xtypz :: Int -> Int -> Int -> Pixel8
xtypz z x y = if isPrime (x*y+z) then 0 else 255

-- rendering

draw :: Dimensions -> Int -> IO ()
draw (w,h) n = do
   savePngImage file $ ImageY8 $ generateImage (xtypz n) w h
   putClrLn G file -- print drawn file to console
   where
   file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_xtyp" ++ show n ++ ".png"

