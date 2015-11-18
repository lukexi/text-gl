{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.GL.Freetype.TextBuffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Monoid
import Data.Foldable
import Data.List (findIndex)
import Data.Maybe
-- import Debug.Trace

import Graphics.GL.Freetype.Types

seqReplace :: (Int, Int) -> Seq a -> Seq a -> Seq a
seqReplace (start, end) xs original = left <> xs <> right
  where
    left  = Seq.take start original
    right = Seq.drop end original

seqRange :: (Int, Int) -> Seq a -> Seq a
seqRange (start, end) = Seq.drop start . Seq.take end


newTextBuffer :: TextBuffer
newTextBuffer = TextBuffer 
  { bufSelection = (0,0)
  , bufColumn    = 0
  , bufText      = mempty
  , bufPath      = mempty
  }

textBufferFromString :: FilePath -> String -> TextBuffer
textBufferFromString filePath string = TextBuffer
  { bufSelection = (0,0)
  , bufColumn    = 0
  , bufText      = Seq.fromList string
  , bufPath      = filePath
  }

stringFromTextBuffer :: TextBuffer -> String
stringFromTextBuffer = toList . bufText

selectionFromTextBuffer :: TextBuffer -> String
selectionFromTextBuffer (TextBuffer selection _ text _) = toList (seqRange selection text)

-- | Returns the number of lines and the number of columns in the longest line
measureTextBuffer :: TextBuffer -> (Int, Int)
measureTextBuffer (TextBuffer _ _ text _) = 
  let lineIndices = Seq.elemIndicesL '\n' text
      numLines = length lineIndices + 1
      numColumns = fst $ foldl' (\(maxCol, lastIndex) thisIndex -> 
        (max maxCol (thisIndex - lastIndex), thisIndex)) 
        (0,0) lineIndices
  in (numColumns, numLines)

-- | Find the column in the current line
-- (in other words, the distance from the previous newline)
currentColumn :: TextBuffer -> Int
currentColumn (TextBuffer (start, _) _ text _) =
  let previousNewline = fromMaybe (-1) . Seq.elemIndexR '\n' . Seq.take start $ text
  in  start - previousNewline

updateCurrentColumn :: TextBuffer -> TextBuffer
updateCurrentColumn buffer = buffer { bufColumn = currentColumn buffer }

insertTextBuffer :: Seq Char -> TextBuffer -> TextBuffer
insertTextBuffer chars buffer@(TextBuffer (start, end) _ text _) = updateCurrentColumn $
  buffer { bufSelection = (newCursor, newCursor), bufText = newText }
  where 
    newText = seqReplace (start, end) chars text
    newCursor = (start + Seq.length chars)

insertChar :: Char -> TextBuffer -> TextBuffer
insertChar char = insertString [char]

insertString :: String -> TextBuffer -> TextBuffer
insertString string = insert (Seq.fromList string)

insert :: Seq Char -> TextBuffer -> TextBuffer
insert chars = insertTextBuffer chars

moveLeft :: TextBuffer -> TextBuffer
moveLeft = updateCurrentColumn . go
  where
    go buffer@(TextBuffer (0, 0) _ _ _) = buffer
    go buffer@(TextBuffer (start, end) _ _ _) 
      | start == end = buffer { bufSelection = (start - 1, start - 1) }
    go buffer@(TextBuffer (start, _) _ _ _) = buffer { bufSelection = (start, start) }

selectLeft :: TextBuffer -> TextBuffer
selectLeft = updateCurrentColumn . go
  where
    go buffer@(TextBuffer (0,       _) _ _ _) = buffer
    go buffer@(TextBuffer (start, end) _ _ _) = buffer { bufSelection = (start - 1, end) }

moveRight :: TextBuffer -> TextBuffer
moveRight = updateCurrentColumn . go
  where
    go buffer@(TextBuffer (start, end) _ text _) 
      | end == Seq.length text = buffer
      | start == end = buffer { bufSelection = (end + 1, end + 1) }
    go buffer@(TextBuffer (_, end) _ _ _) = buffer { bufSelection = (end, end) }


selectRight :: TextBuffer -> TextBuffer
selectRight = updateCurrentColumn . go
  where
    go buffer@(TextBuffer (_, end) _ text _)
      | end == Seq.length text = buffer
    go buffer@(TextBuffer (start, end) _ _ _) = buffer { bufSelection = (start, end + 1) }

backspace :: TextBuffer -> TextBuffer
backspace buffer = 
  let (start, end) = bufSelection buffer
  in insert (Seq.fromList "") (if start == end then selectLeft buffer else buffer)

moveToEnd :: TextBuffer -> TextBuffer
moveToEnd buffer = updateCurrentColumn $
  let end = Seq.length (bufText buffer)
  in buffer { bufSelection = (end, end) }

moveToBeginning :: TextBuffer -> TextBuffer
moveToBeginning buffer = updateCurrentColumn $
  buffer { bufSelection = (0, 0) }

moveDown :: TextBuffer -> TextBuffer
moveDown buffer = 
  let (cursorLocation, _)     = bufSelection buffer
      -- Add an artificial "newline" at -1 to represent the beginning of the document
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
        in buffer { bufSelection = (newCursor, newCursor) }


moveUp :: TextBuffer -> TextBuffer
moveUp buffer =
  let (cursorLocation, _)     = bufSelection buffer
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
        in buffer { bufSelection = (newCursor, newCursor) }

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
