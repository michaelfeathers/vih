

module Input(initInput,getInputChar) where 

import System.IO
import IO

initInput :: IO ()
initInput = do hSetBuffering stdin NoBuffering

getInputChar :: IO Char
getInputChar =
  do hSetEcho stdin False
     c <- getChar
     hSetEcho stdin True
     return c


