
module EditBuffer
     ( EditBuffer(..)
     , Location
     , emptyBuffer
     , enterCommandMode
     , getBufferContents
     , lineCount
     , insertChar, deleteChar
     , insertLineAfter
     , deleteLine
     , moveLeft, moveRight, moveUp, moveDown
     , moveToHome, moveToEnd
     , moveToLineStart, moveToLineEnd
     , showRepresentation
     ) 
where

type Location = (Int, Int)
data EditBuffer = EditBuffer Location String deriving Show

emptyBuffer = EditBuffer (0,0) ""

enterCommandMode :: EditBuffer -> EditBuffer
enterCommandMode = forceLocation 

getBufferContents:: EditBuffer -> String
getBufferContents (EditBuffer _ contents) = contents

lineCount :: EditBuffer -> Int
lineCount (EditBuffer _ contents) = length . lines $ contents

insertChar :: Char -> EditBuffer -> EditBuffer
insertChar ch buffer@(EditBuffer (x, y) contents)
    | ch == '\n' = EditBuffer (0, y+1) newContents
    | otherwise  = EditBuffer (x+1, y) newContents
  where newContents      = before ++ [ch] ++ after
        (before, after)  = split buffer

deleteChar :: EditBuffer -> EditBuffer
deleteChar buffer@(EditBuffer location@(x,y) contents) 
    | (currentLineLength buffer == 0) = buffer
    | otherwise                       = satX 0 (EditBuffer location newContents)
  where newContents     = before ++ (tail after)
        (before, after) = split buffer

insertLineAfter :: EditBuffer -> EditBuffer
insertLineAfter (EditBuffer _ "") = EditBuffer (0,1) "\n"
insertLineAfter (EditBuffer (_,y) contents) = EditBuffer (0,y+1) newContents
  where newContents           = unlines [transform numberedLine | numberedLine <- zip (lines contents) [0..]]
        transform (line, pos) = if pos == y then line ++ "\n" else line  

deleteLine :: EditBuffer ->EditBuffer
deleteLine (EditBuffer location@(_,y) contents) = forceLocation (EditBuffer location newContents)
  where newContents = unlines [ line | (line, pos) <- zip (lines contents) [0..], pos /= y] 

moveLeft, moveRight, moveUp, moveDown :: EditBuffer -> EditBuffer
moveLeft  = saturate (-1, 0)  
moveRight = saturate ( 1, 0)
moveUp    = saturate ( 0,-1)
moveDown  = saturate ( 0, 1)

moveToHome :: EditBuffer -> EditBuffer
moveToHome (EditBuffer _ contents) = EditBuffer (0,0) contents

moveToEnd :: EditBuffer -> EditBuffer
moveToEnd = saturate (lastPos, lastPos)
  where lastPos = (maxBound :: Int) - 1

moveToLineStart :: EditBuffer -> EditBuffer
moveToLineStart (EditBuffer (_,y) contents) = EditBuffer (0,y) contents

moveToLineEnd :: EditBuffer -> EditBuffer
moveToLineEnd buffer@(EditBuffer (_,y) contents) = 
  satX 0 $ (EditBuffer ((currentLineLength buffer), y) contents) 

showRepresentation :: EditBuffer -> String
showRepresentation (EditBuffer location contents) =
  show location ++ " " ++ show contents


forceLocation = saturate (0,0)

currentLine :: EditBuffer -> String
currentLine (EditBuffer _ "") = ""
currentLine buffer@(EditBuffer (_, y) contents) 
  | (y < 0) || (y >= (lineCount buffer))  = ""
  | otherwise                             = (lines contents) !! y

currentLineLength :: EditBuffer -> Int
currentLineLength = length . currentLine

split :: EditBuffer -> (String,String)
split buffer@(EditBuffer _ contents) = splitAt point contents
  where point = absPosition buffer 
  
absPosition :: EditBuffer -> Int
absPosition (EditBuffer (x, y) contents) = 
  (x+) . length . unlines . take y . lines $ contents

saturate :: (Int,Int) -> EditBuffer -> EditBuffer
saturate (adjX,adjY)  = satX adjX . satY adjY

satX :: Int -> EditBuffer -> EditBuffer
satX adjX buffer@(EditBuffer (x,y) contents) =
  EditBuffer (saturateValue (currentLineLength buffer) (x + adjX), y) contents

satY :: Int -> EditBuffer -> EditBuffer
satY adjY buffer@(EditBuffer (x,y) contents) = 
  EditBuffer (x, saturateValue (lineCount buffer) (y + adjY)) contents

saturateValue :: Int -> Int -> Int
saturateValue bound value
  | bound <= 1      = 0
  | value <= 0      = 0
  | value >= bound  = bound - 1 
  | otherwise       = value


