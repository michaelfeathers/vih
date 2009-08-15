
import EditBuffer
import Rendering
import Input
import Char

data EditMode = Command | Insert deriving (Eq,Show)

vih :: IO ()
vih =
  do initInput 
     mainLoop Command emptyBuffer 

mainLoop :: EditMode -> EditBuffer -> IO ()
mainLoop mode buffer@(EditBuffer location contents) = 
  do cls
     render $ getBufferContents buffer
     -- writeAt commandHome (showRepresentation buffer)
     goto location
     ch <- getInputChar
     if mode == Insert
       then 
         case ch of 
           '\ESC' -> mainLoop Command (enterCommandMode buffer)
           _      -> if (isControl ch) && not (ch == '\n')  
                       then mainLoop mode buffer
                       else mainLoop mode (insertChar ch buffer)
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
           'w' -> mainLoop mode (wordForward buffer)
           'x' -> mainLoop mode (deleteChar buffer)
           _   -> mainLoop mode buffer


handleCommandLine :: EditBuffer -> IO ()
handleCommandLine buffer =
  do goto commandHome
     putStr ":"
     command <- getLine
     if head command == 'q'
       then return ()
       else mainLoop Command buffer
