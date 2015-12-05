{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Freetype.TextBuffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Monoid
import Data.Foldable
import Data.List (findIndex)
import Data.Maybe

import Graphics.GL.Freetype.Types

seqReplace :: (Int, Int) -> Seq a -> Seq a -> Seq a
seqReplace (start, end) xs original = left <> xs <> right
  where
    left  = Seq.take start original
    right = Seq.drop end original

seqRange :: (Int, Int) -> Seq a -> Seq a
seqRange (start, end) = Seq.drop start . Seq.take end

-- we insert an invisible '\n' to act as the beginning of the text, 
-- so the initial column becomes 1
newTextBuffer :: TextBuffer
newTextBuffer = TextBuffer 
  { bufSelection = (0,0)
  , bufColumn    = 1 
  , bufText      = mempty
  , bufPath      = mempty
  }

textBufferFromString :: FilePath -> String -> TextBuffer
textBufferFromString filePath string = TextBuffer
  { bufSelection = (0,0)
  , bufColumn    = 1
  , bufText      = Seq.fromList string
  , bufPath      = filePath
  }

stringFromTextBuffer :: TextBuffer -> String
stringFromTextBuffer = toList . bufText

selectionFromTextBuffer :: TextBuffer -> String
selectionFromTextBuffer TextBuffer{..} = toList (seqRange bufSelection bufText)

-- | Returns the number of lines and the number of columns in the longest line
measureText :: Seq Char -> (Int, Int)
measureText text = 
  let lineIndices = Seq.elemIndicesL '\n' text
      numLines = length lineIndices + 1
      numColumns = fst $ foldl' (\(maxCol, lastIndex) thisIndex -> 
        (max maxCol (thisIndex - lastIndex), thisIndex)) 
        (0,0) lineIndices
  in (numColumns, numLines)

-- | Find the column in the current line
-- (in other words, the distance from the previous newline)
currentColumn :: TextBuffer -> Int
currentColumn TextBuffer{..} =
  let previousNewline = fromMaybe (-1) . Seq.elemIndexR '\n' . Seq.take start $ bufText
      (start, _) = bufSelection
  in  start - previousNewline

updateCurrentColumn :: TextBuffer -> TextBuffer
updateCurrentColumn buffer = buffer { bufColumn = currentColumn buffer }

insertTextBuffer :: Seq Char -> TextBuffer -> TextBuffer
insertTextBuffer chars buffer@TextBuffer{..} = updateCurrentColumn $
  buffer { bufSelection = (newCursor, newCursor), bufText = newText }
  where 
    newText = seqReplace (start, end) chars bufText
    newCursor = (start + Seq.length chars)
    (start, end) = bufSelection

insertChar :: Char -> TextBuffer -> TextBuffer
insertChar char = insertString [char]

insertString :: String -> TextBuffer -> TextBuffer
insertString string = insert (Seq.fromList string)

insert :: Seq Char -> TextBuffer -> TextBuffer
insert chars = insertTextBuffer chars

moveCursorTo :: Int -> TextBuffer -> TextBuffer
moveCursorTo i buffer = updateCurrentColumn $ buffer { bufSelection = (i, i) }

moveLeft :: TextBuffer -> TextBuffer
moveLeft buffer = go selection
  where
    selection        = bufSelection buffer
    go (0, 0)        = buffer
    go (start, end)
      | start == end = moveCursorTo (start - 1) buffer
    go (start, _)    = moveCursorTo start buffer

selectLeft :: TextBuffer -> TextBuffer
selectLeft buffer = updateCurrentColumn (go selection)
  where
    selection       = bufSelection buffer
    go (0,       _) = buffer
    go (start, end) = buffer { bufSelection = (start - 1, end) }

moveRight :: TextBuffer -> TextBuffer
moveRight buffer = updateCurrentColumn (go selection)
  where
    selection = bufSelection buffer
    go (start, end)
      | end == Seq.length (bufText buffer) = buffer
      | start == end                       = buffer { bufSelection = (end + 1, end + 1) }
    go (_, end)                            = buffer { bufSelection = (end, end) }


selectRight :: TextBuffer -> TextBuffer
selectRight buffer = updateCurrentColumn (go selection)
  where
    selection = bufSelection buffer
    go (_, end)
      | end == Seq.length (bufText buffer) = buffer
    go (start, end)                        = buffer { bufSelection = (start, end + 1) }

backspace :: TextBuffer -> TextBuffer
backspace buffer = 
  let (start, end) = bufSelection buffer
  in insert (Seq.fromList "") (if start == end then selectLeft buffer else buffer)

moveToEnd :: TextBuffer -> TextBuffer
moveToEnd buffer = 
  let end = Seq.length (bufText buffer)
  in moveCursorTo end buffer

moveToBeginning :: TextBuffer -> TextBuffer
moveToBeginning = moveCursorTo 0

moveDown :: TextBuffer -> TextBuffer
moveDown buffer = 
  let (cursorLocation, _)     = bufSelection buffer
      lineLocations           = Seq.elemIndicesL '\n' (bufText buffer)
  -- If there's no newline beyond the cursor, do nothing
  in case findIndex (>= cursorLocation) lineLocations of
      Nothing -> buffer
      Just nextLineIndex -> 
        let nextLineLocation        = lineLocations !! nextLineIndex
            nextNextLineLocation    = lineLocations !! (min (length lineLocations - 1) $ nextLineIndex + 1)
            currentDistanceFromLeft = bufColumn buffer
            -- Don't jump futher than the next newline location
            newCursor               = min nextNextLineLocation (nextLineLocation + currentDistanceFromLeft)
        -- Don't use moveCursorTo here since we don't want to update the column
        in buffer { bufSelection = (newCursor, newCursor) }


-- We still update the current column for the case
-- where we press up on the first line and return to column 0
moveUp :: TextBuffer -> TextBuffer
moveUp buffer = 
  let (cursorLocation, _)     = bufSelection buffer
      -- Add artificial 'newlines' at the beginning of the document to simplify the algorithm
      lineLocations           = (-1):(-1):Seq.elemIndicesL '\n' (bufText buffer)
  -- If there's no newline beyond the cursor, do nothing
  in case findIndex (>= cursorLocation) lineLocations of
      Nothing -> buffer
      Just nextLineIndex -> 
        let currentLineLocation     = lineLocations !! (nextLineIndex - 1)
            prevLineLocation        = lineLocations !! (nextLineIndex - 2)
            currentDistanceFromLeft = bufColumn buffer
            -- Don't jump futher than the prev newline location
            newCursor               = max 0 $ min currentLineLocation (prevLineLocation + currentDistanceFromLeft)
            -- Update the column iff we have hit up enough times to return to the beginning of the text
            newColumn = if newCursor == 1 then 1 else bufColumn buffer
        in buffer { bufSelection = (newCursor, newCursor), bufColumn = newColumn }



-- main = do
--   flip runStateT newTextBuffer $ do
--     insert "hello"
--     insert " there"
--     insert "\n"
--     moveLeft
--     moveLeft
--     insert "mr."
--     insert "magpie"
--     selectLeft
--     selectLeft
--     insert ""
--     liftIO . print =<< get
