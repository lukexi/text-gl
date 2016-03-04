{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.TextBuffer2 where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import Data.Monoid
import Data.Foldable
import Data.List hiding (insert)
import Data.Maybe

type TextSeq = Seq (Seq Char)

seqHead :: Seq a -> Maybe a
seqHead seqn = case Seq.viewl seqn of
    a :< _ -> Just a
    _ -> Nothing

seqLast :: Seq a -> Maybe a
seqLast seqn = case Seq.viewr seqn of
    _ :> a -> Just a
    _ -> Nothing

seqRange :: (Int, Int) -> Seq a -> Seq a
seqRange (start, end) = Seq.drop start . Seq.take end

textSeqFromString :: String -> TextSeq
textSeqFromString = Seq.fromList . fmap Seq.fromList . lines . fixup
    -- Fix lines returning [] instead of [""] for an empty string
    where fixup "" = "\n"
          fixup other = other

-- | `unlines` adds an unwanted trailing \n
stringFromTextSeq :: TextSeq -> String
stringFromTextSeq = concat . intersperse "\n" . toList . fmap toList

textSeqDimensions :: TextSeq -> (Int, Int)
textSeqDimensions textSeq = (maximum (length <$> textSeq), length textSeq)

data Cursor = Cursor Int Int deriving Show
type Selection = (Cursor, Cursor)

data TextBuffer = TextBuffer 
  { bufSelection    :: !(Maybe Selection)
  , bufColumn       :: !Int
  , bufText         :: !TextSeq
  , bufPath         :: !(Maybe FilePath)
  , bufUndo         :: !(Maybe TextBuffer)
  } deriving Show

newTextBuffer :: TextBuffer
newTextBuffer = TextBuffer 
    { bufSelection = Nothing
    , bufColumn    = 1
    , bufText      = mempty
    , bufPath      = Nothing
    , bufUndo      = Nothing
    }

textBufferWithPath :: FilePath -> String -> TextBuffer
textBufferWithPath filePath string = newTextBuffer 
    { bufText = textSeqFromString string
    , bufPath = Just filePath 
    }

textBufferFromString :: String -> TextBuffer
textBufferFromString string = newTextBuffer 
    { bufText = textSeqFromString string
    }


stringFromTextBuffer :: TextBuffer -> String
stringFromTextBuffer = stringFromTextSeq . bufText

getSelection :: TextBuffer -> Selection
getSelection buffer = fromMaybe (Cursor 0 0, Cursor 0 0) (bufSelection buffer)

selectionFromTextSeq :: TextSeq -> Selection -> String
selectionFromTextSeq textSeq
                     (Cursor startLineNum startColNum, 
                      Cursor endLineNum endColNum) = 
    let selectedLines = seqRange (startLineNum, endLineNum + 1) textSeq
        trimmedLines  = Seq.adjust (Seq.drop startColNum) 0
                      . Seq.adjust (Seq.take endColNum)   (length selectedLines - 1)
                      $ selectedLines
    in stringFromTextSeq trimmedLines

selectionFromTextBuffer :: TextBuffer -> String
selectionFromTextBuffer TextBuffer{..} = fromMaybe "" $ selectionFromTextSeq bufText <$> bufSelection

currentColumn :: TextBuffer -> Int
currentColumn buffer@TextBuffer{..} = startColNum
    where (Cursor _ startColNum, _) = getSelection buffer

pushUndo :: TextBuffer -> TextBuffer
pushUndo buffer = buffer { bufUndo = Just buffer }

undo :: TextBuffer -> TextBuffer
undo buffer = case bufUndo buffer of
    Just prevBuffer -> prevBuffer
    Nothing         -> buffer

updateCurrentColumn :: TextBuffer -> TextBuffer
updateCurrentColumn buffer = buffer { bufColumn = currentColumn buffer }

insertTextSeq :: (Cursor, Cursor) -> TextSeq -> TextSeq -> TextSeq
insertTextSeq sel textSeq origTextSeq = result
  where
    (Cursor startLineNum startColNum, 
     Cursor endLineNum endColNum) = sel
    -- All lines fully before and after the selection
    before = Seq.take startLineNum     origTextSeq
    after  = Seq.drop (endLineNum + 1) origTextSeq
    -- The characters preceding and following the selection
    -- on the line(s) of the selection
    prefix = Seq.take startColNum  (Seq.index origTextSeq startLineNum)
    suffix = Seq.drop endColNum    (Seq.index origTextSeq endLineNum)
    -- Prefix and suffix the newly inserted line(s) with the aforementioned
    -- prefix and suffix characters, and add back the 'before' and 'after' lines
    result = before 
        <> (Seq.adjust (prefix <>) 0 $
            Seq.adjust (<> suffix) (length textSeq - 1) $ 
            textSeq)
        <> after 

insertTextBuffer :: TextSeq -> TextBuffer -> TextBuffer
insertTextBuffer textSeq buffer = updateCurrentColumn $ newBuffer
    { bufText      = newText
    , bufSelection = Just (newCursor, newCursor)
    }
    where
        newBuffer = pushUndo buffer
        newText   = insertTextSeq (getSelection buffer) textSeq (bufText buffer)
        newCursor = Cursor newLineNum newColNum
        newLineNum = startLineNum + (length textSeq - 1)
        newColNum = if newLineNum == startLineNum then startColNum + lastLen else lastLen
        lastLen = fromMaybe 0 (length <$> seqLast textSeq)
        (Cursor startLineNum startColNum, _) = getSelection buffer


insertChar :: Char -> TextBuffer -> TextBuffer
insertChar char = insertString [char]

insertString :: String -> TextBuffer -> TextBuffer
insertString string = insert (textSeqFromString string)

insert :: TextSeq -> TextBuffer -> TextBuffer
insert chars = insertTextBuffer chars

moveCursorTo :: Int -> TextBuffer -> TextBuffer
moveCursorTo i buffer = updateCurrentColumn $ buffer { bufSelection = Just (Cursor i i, Cursor i i) }


testSelection :: (Cursor, Cursor)
testSelection = (Cursor 0 1, Cursor 1 2)

testBuffer :: TextBuffer
testBuffer = (textBufferFromString "hello\nworld\ngreat") {bufSelection=Just testSelection}

test1 :: TextBuffer
test1 = insertString "" testBuffer

test2 :: TextSeq
test2 = insertTextSeq testSelection
    (textSeqFromString "ill\nhouse\nbo")
    (textSeqFromString "hello\nworld\ngreat")

test3 :: TextSeq
test3 = insertTextSeq testSelection
    (textSeqFromString "bo")
    (textSeqFromString "hello\nworld\ngreat")

test4 :: TextSeq
test4 = insertTextSeq testSelection
    (textSeqFromString "y")
    (textSeqFromString "baggins")

test5 :: TextBuffer
test5 = insertString "y"
    (textBufferFromString "hello\nworld") { bufSelection = Just (Cursor 0 3, Cursor 0 4) }

test6 :: TextSeq
test6 = insertTextSeq (Cursor 0 3, Cursor 0 4)
    (textSeqFromString "y")
    (textSeqFromString "hello\nworld")
