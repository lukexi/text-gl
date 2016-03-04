{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.TextBuffer2 where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
-- import Data.Monoid
import Data.Foldable
-- import Data.List (findIndex)
-- import Data.Maybe

type TextSeq = Seq (Seq Char)

seqRange :: (Int, Int) -> Seq a -> Seq a
seqRange (start, end) = Seq.drop start . Seq.take end

linesFromString :: String -> Seq (Seq Char)
linesFromString = Seq.unfoldl (\string -> 
    case Seq.elemIndexR '\n' string of
        Just nextNewLine -> 
            let (line, rest) = Seq.splitAt nextNewLine string
            in Just (line, rest)
        Nothing -> Nothing
    ) . Seq.fromList

data Cursor = Cursor Int Int deriving Show

data TextBuffer = TextBuffer 
  { bufSelection    :: !(Maybe (Cursor, Cursor))
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
    { bufText = linesFromString string
    , bufPath = Just filePath 
    }

textBufferFromString :: String -> TextBuffer
textBufferFromString string = newTextBuffer 
    { bufText = linesFromString string
    }


stringFromTextBuffer :: TextBuffer -> String
stringFromTextBuffer = stringFromLines . bufText

stringFromLines :: TextSeq -> String
stringFromLines = unlines . toList . fmap toList

selectionFromTextBuffer :: TextBuffer -> String
selectionFromTextBuffer TextBuffer{..} = case bufSelection of 
    Just (Cursor startLineNum startColNum, Cursor endLineNum endColNum) -> 
        let selectedLines = seqRange (startLineNum, endLineNum) bufText
            trimmedLines = Seq.adjust (Seq.drop startColNum) 0
                         . Seq.adjust (Seq.take endColNum)   (Seq.length selectedLines)
                         $ selectedLines
        in stringFromLines trimmedLines
    Nothing -> mempty


