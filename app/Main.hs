import System.IO
import System.Environment
import System.Directory
import Data.Numbers.Primes
import qualified Data.Map as Map
import Color
import Codec.Picture
import Control.Concurrent
import Control.Parallel.Strategies

-- pixels

black :: Pixel8
black = 0

white :: Pixel8
white = 255

-- dimensions

type Dimensions = (Int, Int)

-- function

xtypz :: Int -> Int -> Int -> Pixel8
xtypz z x y = if isPrime (x*y+z) then black else white

-- rendering

draw :: Dimensions -> Int -> IO ()
draw d n = let
   (w, h) = d
   file = "io/" ++ show w ++ "x" ++ show h ++ "pixel_xtyp" ++ show n ++ ".png"
   in do 
   done <- doesFileExist file
   if done then 
      putClrLn W (file ++ " skipped") -- print existing file to console
   else do 
      savePngImage file $ ImageY8 $ generateImage (xtypz n) w h
      putClrLn G file -- print drawn file to console

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
      in do
      createDirectoryIfMissing True "io"
      draw_list (w, h) list
   else do
      putClr R "use with args:"
      putClrLn W "whidth height from to"
      pure ()