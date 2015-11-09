{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TextBuffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Monoid
import Control.Monad.State
import Data.String
import Data.Foldable
import Data.Maybe
import Data.List (findIndex)
import Debug.Trace
import Text.Printf

instance IsString (Seq Char) where
  fromString = Seq.fromList

seqReplace :: (Int, Int) -> Seq a -> Seq a -> Seq a
seqReplace (start, end) xs original = left <> xs <> right
  where
    left = Seq.take start original
    right = Seq.drop end original

data Buffer = Buffer 
  { bufSelection :: !(Int, Int)
  , bufText      :: !(Seq Char)
  } deriving Show

newBuffer :: Buffer
newBuffer = Buffer 
  { bufSelection = (0,0)
  , bufText = ""
  }

bufferFromString :: String -> Buffer
bufferFromString string = Buffer
  { bufSelection = (0,0)
  , bufText = fromString string
  }

stringFromBuffer :: Buffer -> String
stringFromBuffer = toList . bufText

insertBuffer :: Seq Char -> Buffer -> Buffer
insertBuffer chars (Buffer (start, end) text) = 
  Buffer { bufSelection = (newCursor, newCursor), bufText = newText }
  where 
    newText = seqReplace (start, end) chars text
    newCursor = (start + Seq.length chars)

insertChar :: MonadState Buffer m => Char -> m ()
insertChar char = insertString [char]

insertString :: MonadState Buffer m => String -> m ()
insertString string = insert (fromString string)

insert :: MonadState Buffer m => Seq Char -> m ()
insert chars = modify' (insertBuffer chars)

moveLeft :: MonadState Buffer m => m ()
moveLeft = modify' go
  where
    go buffer@(Buffer (0, 0) _) = buffer
    go buffer@(Buffer (start, end) _) 
      | start == end = buffer { bufSelection = (start - 1, start - 1) }
    go buffer@(Buffer (start, _) _) = buffer { bufSelection = (start, start) }

selectLeft :: MonadState Buffer m => m ()
selectLeft = modify' go
  where
    go buffer@(Buffer (0,       _) _) = buffer
    go buffer@(Buffer (start, end) _) = buffer { bufSelection = (start - 1, end) }

moveRight :: MonadState Buffer m => m ()
moveRight = modify' go
  where
    go buffer@(Buffer (start, end) text) 
      | end == Seq.length text = buffer
      | start == end = buffer { bufSelection = (end + 1, end + 1) }
    go buffer@(Buffer (_, end) _) = buffer { bufSelection = (end, end) }


selectRight :: MonadState Buffer m => m ()
selectRight = modify' go
  where
    go buffer@(Buffer (_, end) text)
      | end == Seq.length text = buffer
    go buffer@(Buffer (start, end) _) = buffer { bufSelection = (start, end + 1) }

backspace :: MonadState Buffer m => m ()
backspace = do
  (start, end) <- gets bufSelection
  when (start == end) selectLeft
  insert ""

moveToEnd :: MonadState Buffer m => m ()
moveToEnd = modify' $ \buffer -> 
  let end = Seq.length (bufText buffer)
  in buffer { bufSelection = (end, end) }

moveToBeginning :: MonadState Buffer m => m ()
moveToBeginning = modify' $ \buffer -> 
  buffer { bufSelection = (0, 0) }

moveDown :: MonadState Buffer m => m ()
moveDown = modify' $ \buffer ->
  let (cursorLocation, _)     = bufSelection buffer
      -- Add an artificial "newline" at -1 to represent the beginning of the document
      lineLocations           = (-1):Seq.elemIndicesL '\n' (bufText buffer)
  -- If there's no newline beyond the cursor, do nothing
  in case findIndex (>= cursorLocation) lineLocations of
      Nothing -> buffer
      Just nextLineIndex -> 
        -- Thanks to the (-1) fake newline, we can always count on nextLineIndex begin at least 1
        let currentLineLocation     = lineLocations !! (nextLineIndex - 1)
            nextLineLocation        = lineLocations !! nextLineIndex
            nextNextLineLocation    = lineLocations !! (nextLineIndex + 1)
            currentDistanceFromLeft = cursorLocation - currentLineLocation
            -- Don't jump futher than the next newline location
            newCursor               = min nextNextLineLocation (nextLineLocation + currentDistanceFromLeft)
        in buffer { bufSelection = (newCursor, newCursor) }


moveUp :: MonadState Buffer m => m ()
moveUp = modify' $ \buffer ->
  let (cursorLocation, _)     = bufSelection buffer
      lineLocations           = (-1):(-1):Seq.elemIndicesL '\n' (bufText buffer)
  -- If there's no newline beyond the cursor, do nothing
  in case findIndex (>= cursorLocation) lineLocations of
      Nothing -> buffer
      Just nextLineIndex -> 
        let currentLineLocation     = lineLocations !! (nextLineIndex - 1)
            prevLineLocation        = lineLocations !! (nextLineIndex - 2)
            currentDistanceFromLeft = cursorLocation - currentLineLocation
            -- Don't jump futher than the prev newline location
            newCursor               = max 0 $ min currentLineLocation (prevLineLocation + currentDistanceFromLeft)
        in buffer { bufSelection = (newCursor, newCursor) }

-- main = do
--   flip runStateT newBuffer $ do
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
