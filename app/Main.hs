import System.IO
import System.Environment
import System.Directory
import Data.Numbers.Primes
import Color
import Codec.Picture

-- pixels

black :: Pixel8
black = 0

white :: Pixel8
white = 255

-- dimensions

type Dimensions = (Int,Int)

-- function

xtypz :: Int -> Int -> Int -> Pixel8
xtypz z x y = if isPrime (x*y+z) then black else white

-- rendering

draw :: Dimensions -> Int -> IO ()
draw d n = do
   createDirectoryIfMissing True "io"
   done <- doesFileExist file
   if done then
      putClrLn W (file ++ " skipped") -- print existing file to console
   else do
      savePngImage file $ ImageY8 $ generateImage (xtypz n) w h
      putClrLn G file -- print drawn file to console
   where
      (w,h) = d
      file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_xtyp" ++ show n ++ ".png"

-- draw many xtyp [z]

draw_list :: Dimensions -> [Int] -> IO ()
draw_list d list = do
   mapM_ (draw d) list

-- prompt

main :: IO ()
main = do
   args <- getArgs
   if length args == 4 then let
      (w:h:from:to:_) = map read args
      list = [from .. to]
      in
      draw_list (w,h) list
   else do
      putClr R "args:"
      putClrLn W "width height from to"
      pure ()
