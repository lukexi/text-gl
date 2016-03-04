{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.TextBuffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import Data.Monoid
import Data.Foldable
import Data.List hiding (insert)
import Data.Maybe
import Graphics.GL.TextBuffer.Types


zeroWidth :: Selection -> Maybe Cursor
zeroWidth (start, end)
    | start == end = Just start
    | otherwise = Nothing

cursorEqual :: Cursor -> Selection -> Bool
cursorEqual cursor (selectionStart, selectionEnd) = cursor == selectionStart && cursor == selectionEnd

cursorWithin :: Cursor -> Selection -> Bool
cursorWithin (Cursor lineNum colNum) (Cursor startLineNum startColNum, Cursor endLineNum endColNum) =  
    lineNum >= startLineNum && lineNum <= endLineNum &&
    colNum >= startColNum   && colNum < endColNum

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

moveTo :: Cursor -> TextBuffer -> TextBuffer
moveTo cursor buffer = updateCurrentColumn $ buffer { bufSelection = Just (cursor, cursor) }

moveLeft :: TextBuffer -> TextBuffer
moveLeft buffer = check selection
  where
    selection        = getSelection buffer
    check (start, end)
      | start == end = moveTo (cursorLeft start buffer) buffer
    check (start, _) = moveTo start buffer

moveDown :: TextBuffer -> TextBuffer
moveDown buffer = check selection
  where
    selection        = getSelection buffer
    check (start, end)
      | start == end = moveTo (cursorDown start buffer) buffer
    check (_, end) = moveTo end buffer

moveUp :: TextBuffer -> TextBuffer
moveUp buffer = check selection
  where
    selection        = getSelection buffer
    check (start, end)
      | start == end = moveTo (cursorUp start buffer) buffer
    check (start, _) = moveTo start buffer

cursorLeft :: Cursor -> TextBuffer -> Cursor
cursorLeft (Cursor 0 0) _buffer = Cursor 0 0
cursorLeft (Cursor l 0) buffer  = cursorToEndOfLine (l - 1) buffer
cursorLeft (Cursor l c) _buffer = Cursor l (c - 1)

cursorUp :: Cursor -> TextBuffer -> Cursor
cursorUp (Cursor 0 0) _buffer = Cursor 0 0
cursorUp (Cursor l c) buffer  = cursorToColumnInLine (l - 1) c buffer

cursorDown :: Cursor -> TextBuffer -> Cursor
cursorDown cursor@(Cursor l c) buffer
  | l == maxLine = cursor
  | otherwise    = cursorToColumnInLine (l + 1) c buffer
  where
    maxLine = length text - 1
    text = bufText buffer

cursorToColumnInLine l c buffer = Cursor l (min c lineLen)
  where
    lineLen = lineLength l (bufText buffer)

cursorToEndOfLine :: LineNum -> TextBuffer -> Cursor
cursorToEndOfLine l buffer = Cursor l lineLen
  where
    lineLen = lineLength l (bufText buffer)

selectLeft :: TextBuffer -> TextBuffer
selectLeft buffer = updateCurrentColumn (go selection)
  where
    selection       = getSelection buffer
    go (start, end) = buffer { bufSelection = Just (cursorLeft start buffer, end) }

selectRight :: TextBuffer -> TextBuffer
selectRight buffer = updateCurrentColumn (go selection)
  where
    selection       = getSelection buffer
    go (start, end) = buffer { bufSelection = Just (start, cursorRight end buffer) }

moveToStartOfLine :: LineNum -> TextBuffer -> TextBuffer
moveToStartOfLine l buffer = moveTo (cursorToStartOfLine l) buffer

cursorToStartOfLine :: LineNum -> Cursor
cursorToStartOfLine l = Cursor l 0

moveToEndOfLine :: LineNum -> TextBuffer -> TextBuffer
moveToEndOfLine l buffer = moveTo (cursorToEndOfLine l buffer) buffer

lineLength :: LineNum -> TextSeq -> Int
lineLength l textSeq = length (Seq.index textSeq l)

moveRight :: TextBuffer -> TextBuffer
moveRight buffer = check selection
  where
    selection       = getSelection buffer
    check (start, end)
      | start == end = moveTo (cursorRight start buffer) buffer
    check (_, end) = moveTo end buffer

cursorRight :: Cursor -> TextBuffer -> Cursor
cursorRight cursor@(Cursor l c) buffer
  | l == maxLine &&
    c == lineLength l text = cursor
  | c == lineLength l text = cursorToStartOfLine (l + 1)
  | otherwise = (Cursor l (c + 1))
  where
    maxLine = length text - 1
    text = bufText buffer

backspace :: TextBuffer -> TextBuffer
backspace buffer = 
    let (start, end) = getSelection buffer
    in insertString "" (if start == end then selectLeft buffer else buffer)

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
