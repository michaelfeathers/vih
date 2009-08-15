

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

render :: EditBuffer -> IO ()
render (EditBuffer topLine (x,y) contents) = do 
  writeAt home $ window screenLines 
  goto (x, y - topLine)
    where window       = unlines . take yExtent . drop topLine  
          screenLines  = (lines contents) ++ (repeat "~")

goto :: Location -> IO ()
goto (x,y) =
  putStr ("\ESC[" ++ show (y + 1) ++ "; " ++ show (x + 1) ++ "H")

writeAt :: Location -> String -> IO ()
writeAt location xs = do
  goto location
  putStr xs

