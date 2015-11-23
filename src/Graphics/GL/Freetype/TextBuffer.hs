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

import Graphics.GL.Pal hiding (trace)
import System.IO.Unsafe
-- import Debug.Trace

import Control.Monad.Trans

import Graphics.GL.Freetype.Types
import Graphics.GL.Freetype.API

seqReplace :: (Int, Int) -> Seq a -> Seq a -> Seq a
seqReplace (start, end) xs original = left <> xs <> right
  where
    left  = Seq.take start original
    right = Seq.drop end original

seqRange :: (Int, Int) -> Seq a -> Seq a
seqRange (start, end) = Seq.drop start . Seq.take end


newTextBuffer :: Font -> TextBuffer
newTextBuffer font = TextBuffer 
  { bufSelection = (0,0)
  , bufColumn    = 0
  , bufText      = mempty
  , bufPath      = mempty
  , bufFont      = font
  }

textBufferFromString :: Font -> FilePath -> String -> TextBuffer
textBufferFromString font filePath string = TextBuffer
  { bufSelection = (0,0)
  , bufColumn    = 0
  , bufText      = Seq.fromList string
  , bufPath      = filePath
  , bufFont      = font
  }

stringFromTextBuffer :: TextBuffer -> String
stringFromTextBuffer = toList . bufText

selectionFromTextBuffer :: TextBuffer -> String
selectionFromTextBuffer TextBuffer{..} = toList (seqRange bufSelection bufText)

-- | Returns the number of lines and the number of columns in the longest line
measureTextBuffer :: TextBuffer -> (Int, Int)
measureTextBuffer TextBuffer{bufText=text} = 
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

moveLeft :: TextBuffer -> TextBuffer
moveLeft buffer = updateCurrentColumn (go selection)
  where
    selection        = bufSelection buffer
    go (0, 0)        = buffer
    go (start, end)
      | start == end = buffer { bufSelection = (start - 1, start - 1) }
    go (start, _)    = buffer { bufSelection = (start, start) }

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

getGlyphKerning' :: Glyph -> Char -> Float
getGlyphKerning' glyph character = unsafePerformIO (getGlyphKerning (glyGlyphPtr glyph) character)

-- | Recalculates the character indices and glyph offsets of a TextBuffer 
-- and writes them into the UBO
updateIndicesAndOffsets :: MonadIO m => TextBuffer -> m ()
updateIndicesAndOffsets textBuffer = do
  let (indices, offsets) = calculateIndicesAndOffsets textBuffer
      Font{..} = bufFont textBuffer
  -- liftIO$print (reverse indexes)
  bufferSubData fntIndexBuffer  (reverse indices)
  bufferSubData fntOffsetBuffer (concatMap toList . reverse $ offsets)

-- | Recalculates the character indices and glyph offsets of a TextBuffer
calculateIndicesAndOffsets :: TextBuffer -> ([GLint], [V2 GLfloat])
calculateIndicesAndOffsets TextBuffer{..} = 
  let blockGlyph         = glyphForChar blockChar
      cursorGlyph        = glyphForChar cursorChar
      glyphForChar       = fntGlyphForChar bufFont
      pointSize          = fntPointSize bufFont
      (selStart, selEnd) = bufSelection
      renderChar (charNum, lineNum, lastXOffset, maybeLastChar, indicesF, offsetsF) character = 
            -- Render newlines as spaces
        let glyph   = glyphForChar (if character == '\n' then ' ' else character)

            -- Find the optimal kerning between this character and the last one rendered (if any)
            kerning = maybe 0 (getGlyphKerning' glyph) maybeLastChar

            -- Adjust the character's x offset to nestle against the previous character
            charXOffset = lastXOffset + kerning
            nextXOffset = charXOffset + gmAdvanceX (glyMetrics glyph)
            charOffset  = V2 charXOffset (-lineNum * pointSize)
            (newIndices, newOffsets) 
              | charNum == selStart && charNum == selEnd =
                let indices' = glyIndex cursorGlyph : glyIndex glyph : indicesF :: [GLint]
                    offsets' = charOffset           : charOffset     : offsetsF :: [V2 GLfloat]
                in (indices', offsets')
              | charNum >= selStart && charNum < selEnd = 
                let indices' = glyIndex blockGlyph : glyIndex glyph : indicesF :: [GLint]
                    offsets' = charOffset          : charOffset     : offsetsF :: [V2 GLfloat]
                in (indices', offsets')
              | otherwise =
                let indices' = glyIndex glyph : indicesF :: [GLint]
                    offsets' = charOffset     : offsetsF :: [V2 GLfloat]
                in (indices', offsets')
        
        in if character == '\n'
            then (charNum + 1, lineNum + 1,           0, Nothing       , newIndices, newOffsets)
            else (charNum + 1, lineNum    , nextXOffset, Just character, newIndices, newOffsets)
      (_, _, _, _, indices, offsets) = foldl' renderChar (0, 0, 0, Nothing, [], []) bufText
  in (indices, offsets)

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
