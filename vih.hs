
import EditBuffer
import Rendering
import Input
import Char
import Control.Exception

data EditMode = Command | Insert deriving (Eq,Show)

vih :: IO ()
vih =
  do initInput 
     mainLoop Command emptyBuffer 

mainLoop :: EditMode -> EditBuffer -> IO ()
mainLoop mode previousBuffer = 
  do cls
     buffer <- return (frame previousBuffer) 
     render buffer
     -- writeAt commandHome (showRepresentation buffer)
     ch <- getInputChar
     if mode == Insert
       then 
         case ch of 
           '\ESC' -> mainLoop Command (enterCommandMode buffer)
           _      -> if isInputChar ch 
                       then mainLoop mode (insertChar ch buffer)
                       else mainLoop mode buffer
       else
         case ch of
           ':' -> handleCommandLine buffer
           'i' -> mainLoop Insert buffer
           'h' -> mainLoop mode (moveLeft buffer)
           'j' -> mainLoop mode (moveDown buffer)
           'k' -> mainLoop mode (moveUp buffer) 
           'l' -> mainLoop mode (moveRight buffer)
           'd' -> do nextCh <- getInputChar
                     if nextCh == 'd' 
                        then mainLoop mode (deleteLine buffer) 
                        else mainLoop mode buffer
           'g' -> do nextCh <- getInputChar
                     if nextCh == 'g' 
                        then mainLoop mode (moveToHome buffer) 
                        else mainLoop mode buffer 
           'G' -> mainLoop mode (moveToEnd buffer)
           '0' -> mainLoop mode (moveToLineStart buffer)
           '$' -> mainLoop mode (moveToLineEnd buffer)
           'o' -> mainLoop Insert (insertLineAfter buffer)
           'r' -> do nextCh <- getInputChar 
                     if isInputChar nextCh
                       then mainLoop mode (replaceChar nextCh buffer)
                       else mainLoop mode buffer
           'w' -> mainLoop mode (wordForward buffer)
           'x' -> mainLoop mode (deleteChar buffer)
           _   -> mainLoop mode buffer


-- aborts when line command is not parseable.  Fix with exception catch
handleCommandLine :: EditBuffer -> IO ()
handleCommandLine buffer =
  do goto commandHome
     putStr ":"
     command <- getLine
     if head command == 'q'
       then return ()
       else if isDigit (head command)
         then mainLoop Command (moveToLine (read command :: Int) buffer)
         else mainLoop Command buffer


isInputChar :: Char -> Bool
isInputChar ch = (not (isControl ch)) || (ch == '\n')

