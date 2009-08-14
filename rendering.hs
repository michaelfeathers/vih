

module Rendering
     ( render
     , cls
     , goto
     , home
     , commandHome
     , writeAt
     ) 
where

import EditBuffer
import IO

yExtent :: Int
yExtent = 40

home :: Location
home = (0,0)

commandHome :: Location
commandHome = (0, yExtent + 1)

cls :: IO ()
cls = putStr "\ESC[2J"

render :: String -> IO ()
render s = do 
  writeAt home $ frame screenLines 
    where frame       = unlines . take yExtent  
          screenLines = (lines s) ++ (repeat "~")

goto :: Location -> IO ()
goto (x,y) =
  putStr ("\ESC[" ++ show (y + 1) ++ "; " ++ show (x + 1) ++ "H")

writeAt :: Location -> String -> IO ()
writeAt location xs = do
  goto location
  putStr xs

