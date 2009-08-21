
module EditBuffer
     ( EditBuffer(..)
     , Location
     , emptyBuffer
     , enterCommandMode
     , getBufferContents
     , lineCount
     , insertChar, deleteChar, replaceChar
     , insertLineAfter
     , deleteLine
     , moveLeft, moveRight, moveUp, moveDown
     , moveToHome, moveToEnd, moveToLine
     , moveToLineStart, moveToLineEnd
     , wordForward, wordBackward
     , frame
     , showRepresentation
     ) 
where

import Char

type Location = (Int, Int)
data EditBuffer = EditBuffer Int Location String deriving (Eq,Show)

emptyBuffer = EditBuffer 0 (0,0) ""

enterCommandMode :: EditBuffer -> EditBuffer
enterCommandMode = forceLocation 

getBufferContents:: EditBuffer -> String
getBufferContents (EditBuffer _ _ contents) = contents

lineCount :: EditBuffer -> Int
lineCount (EditBuffer _ _ contents) = length . lines $ contents

insertChar :: Char -> EditBuffer -> EditBuffer
insertChar ch buffer@(EditBuffer topLine (x, y) contents)
    | ch == '\n' = EditBuffer topLine (0, y+1) newContents
    | otherwise  = EditBuffer topLine (x+1, y) newContents
  where newContents      = before ++ [ch] ++ after
        (before, after)  = split buffer

deleteChar :: EditBuffer -> EditBuffer
deleteChar buffer@(EditBuffer topLine location@(x,y) contents) 
    | (currentLineLength buffer == 0) = buffer
    | otherwise                       = satX 0 (EditBuffer topLine location newContents)
  where newContents     = before ++ (tail after)
        (before, after) = split buffer

replaceChar :: Char -> EditBuffer -> EditBuffer
replaceChar replacementChar buffer@(EditBuffer topLine location contents) =
  let newContents  = map f . numberedElements $ contents 
      f (ch, pos)  = if pos == (absPosition buffer) then replacementChar else ch 
  in EditBuffer topLine location newContents
 
insertLineAfter :: EditBuffer -> EditBuffer
insertLineAfter (EditBuffer topLine _ "") = EditBuffer topLine (0,1) "\n"
insertLineAfter (EditBuffer topLine (_,y) contents) = EditBuffer topLine (0,y+1) newContents
  where newContents   = unlines . map f .  numberedLines $ contents
        f (line, pos) = if pos == y then line ++ "\n" else line  

deleteLine :: EditBuffer ->EditBuffer
deleteLine (EditBuffer topLine location@(_,y) contents) = forceLocation (EditBuffer topLine location newContents)
  where newContents = unlines [ line | (line, pos) <- numberedLines contents, pos /= y] 

moveLeft, moveRight, moveUp, moveDown :: EditBuffer -> EditBuffer
moveLeft  = saturate (-1, 0)  
moveRight = saturate ( 1, 0)
moveUp    = saturate ( 0,-1)
moveDown  = saturate ( 0, 1)

moveToHome :: EditBuffer -> EditBuffer
moveToHome (EditBuffer topLine _ contents) = EditBuffer topLine (0,0) contents

moveToEnd :: EditBuffer -> EditBuffer
moveToEnd = saturate (lastPos, lastPos)
  where lastPos = (maxBound :: Int) - 1

moveToLine :: Int -> EditBuffer -> EditBuffer
moveToLine lineNumber (EditBuffer topLine (x,y) contents) =
  forceLocation (EditBuffer topLine (x, lineNumber) contents)

moveToLineStart :: EditBuffer -> EditBuffer
moveToLineStart (EditBuffer topLine (_,y) contents) = EditBuffer topLine (0,y) contents

moveToLineEnd :: EditBuffer -> EditBuffer
moveToLineEnd buffer@(EditBuffer topLine (_,y) contents) = 
  satX 0 $ (EditBuffer topLine ((currentLineLength buffer), y) contents) 

wordForward :: EditBuffer -> EditBuffer
wordForward buffer@(EditBuffer topLine _ contents) = 
  case dropSpaces . dropWord . drop (absPosition buffer) . numberedElements $ contents of
    []            -> buffer
    ((_,pos) : _) -> EditBuffer topLine (locationFromPosition pos contents) contents

wordBackward :: EditBuffer -> EditBuffer
wordBackward buffer@(EditBuffer topLine _ contents) = 
  case dropWord . dropSpaces . reverse . take (absPosition buffer) . numberedElements $ contents of
    []            -> EditBuffer topLine (locationFromPosition 0 contents) contents
    ((_,pos) : _) -> EditBuffer topLine (locationFromPosition (pos+1) contents) contents

frame :: EditBuffer -> EditBuffer
frame buffer@(EditBuffer topLine (x,y) contents) 
  | y > topLine + 40 = EditBuffer (y - 40) (x,y) contents
  | y < topLine      = EditBuffer y (x,y) contents
  | otherwise        = buffer 

showRepresentation :: EditBuffer -> String
showRepresentation (EditBuffer topLine location contents) =
  show topLine ++ " " ++ show location ++ " " ++ show contents


forceLocation = saturate (0,0)

currentLine :: EditBuffer -> String
currentLine (EditBuffer _ _ "") = ""
currentLine buffer@(EditBuffer _ (_, y) contents) 
  | (y < 0) || (y >= (lineCount buffer))  = ""
  | otherwise                             = (lines contents) !! y

currentLineLength :: EditBuffer -> Int
currentLineLength = length . currentLine

split :: EditBuffer -> (String,String)
split buffer@(EditBuffer _ _ contents) = splitAt point contents
  where point = absPosition buffer 
  
absPosition :: EditBuffer -> Int
absPosition (EditBuffer _ (x, y) contents) = 
  (x+) . length . unlines . take y . lines $ contents

locationFromPosition :: Int -> String -> Location
locationFromPosition pos contents =
  let foreLines = init . lines . take (pos + 1) $ contents
      x         = pos - (length $ unlines foreLines) 
      y         = length foreLines
  in (x, y)

isPunct :: Char -> Bool
isPunct ch = isAscii ch && not (isAlphaNum ch) && not (isSpace ch) && not (isControl ch)  

dropWord :: [(Char,a)] -> [(Char,a)]
dropWord [] = []
dropWord all@((ch,_):_) 
  | isPunct ch    = dropPuncts all
  | isAlphaNum ch = dropAlphaNums all 
  | otherwise     = all

dropPuncts, dropSpaces, dropAlphaNums :: [(Char,a)] -> [(Char,a)]
dropPuncts = dropInNumbered isPunct 
dropSpaces = dropInNumbered isSpace
dropAlphaNums = dropInNumbered isAlphaNum

dropInNumbered :: (Char -> Bool) -> [(Char,a)] -> [(Char,a)]
dropInNumbered f = dropWhile (\(ch,_) -> f ch)

saturate :: (Int,Int) -> EditBuffer -> EditBuffer
saturate (adjX,adjY)  = satX adjX . satY adjY

satX :: Int -> EditBuffer -> EditBuffer
satX adjX buffer@(EditBuffer topLine (x,y) contents) =
  EditBuffer topLine (saturateValue (currentLineLength buffer) (x + adjX), y) contents

satY :: Int -> EditBuffer -> EditBuffer
satY adjY buffer@(EditBuffer topLine (x,y) contents) = 
  EditBuffer topLine (x, saturateValue (lineCount buffer) (y + adjY)) contents

saturateValue :: Int -> Int -> Int
saturateValue bound value
  | bound <= 1      = 0
  | value <= 0      = 0
  | value >= bound  = bound - 1 
  | otherwise       = value

numberedElements :: [a] -> [(a,Int)]
numberedElements = (flip zip) [0..]

numberedLines :: String -> [(String,Int)]
numberedLines = numberedElements . lines

