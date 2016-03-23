{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Graphics.GL.TextBuffer.TextBuffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import Data.Monoid
import Data.Foldable
import Data.List hiding (insert)
import Data.Maybe
import Graphics.GL.TextBuffer.Types

isZeroWidth :: Selection -> Bool
isZeroWidth (start, end) = start == end

getZeroWidth :: Selection -> Maybe Cursor
getZeroWidth cursor@(start, _)
    | isZeroWidth cursor = Just start
    | otherwise          = Nothing

cursorEqual :: Cursor -> Selection -> Bool
cursorEqual cursor (selectionStart, selectionEnd) = cursor == selectionStart && cursor == selectionEnd

cursorWithin :: Cursor -> Selection -> Bool
cursorWithin (Cursor lineNum colNum) 
             sel@(Cursor startLineNum startColNum, Cursor endLineNum endColNum) = 
    not (isZeroWidth sel) &&
    (lineNum >= startLineNum && lineNum <= endLineNum) &&
    if
        | lineNum == startLineNum && lineNum == endLineNum ->
            colNum >= startColNum && colNum < endColNum
        | lineNum == startLineNum -> colNum >= startColNum
        | lineNum == endLineNum   -> colNum < endColNum
        | otherwise -> True

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
    -- FIXME: this works for single newline insertion only
    -- Write a lines replacement that does what we want?
          fixup "\n" = "\n\n"
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

setTextFromString :: String -> TextBuffer -> TextBuffer 
setTextFromString string buffer = 
    pushUndo . validateSelection $ 
        buffer
          { bufText = textSeqFromString string
          }

validateSelection :: TextBuffer -> TextBuffer
validateSelection textBuffer@TextBuffer{..} = case bufSelection of
    Nothing -> textBuffer
    Just (cursorBegin, cursorEnd) -> updateCurrentColumn $ textBuffer 
        { bufSelection = Just ( validateCursor cursorBegin textBuffer
                              , validateCursor cursorEnd   textBuffer)
        }

-- | Verify that the given cursor is within the boundaries of the text buffer
validateCursor :: Cursor -> TextBuffer -> Cursor
validateCursor cursor@(Cursor lineNum colNum) textBuffer@TextBuffer{..} 
    | lineNum >= maxLine    = cursorToEndOfLine maxLine textBuffer
    | colNum  >= maxLineCol = cursorToEndOfLine lineNum textBuffer
    | otherwise             = cursor
    where maxLine      = length bufText - 1
          maxLineCol   = lineLength lineNum bufText

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

-- | Insert some text into an existing buffer.
-- Pushes an undo, updates the current column, and updates the cursor.
insertTextBuffer :: TextSeq -> TextBuffer -> TextBuffer
insertTextBuffer textSeq buffer = updateCurrentColumn $ newBuffer
    { bufText      = newText
    , bufSelection = Just (newCursor, newCursor)
    }
    where
        newBuffer = pushUndo buffer
        newText   = insertTextSeq (getSelection buffer) textSeq (bufText buffer)
        -- Calculate where the cursor should be after inserting the text
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
moveTo cursor buffer = updateCurrentColumn $ moveTo' cursor buffer

moveTo' :: Cursor -> TextBuffer -> TextBuffer
moveTo' cursor buffer = buffer { bufSelection = Just (cursor, cursor) }

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
      | start == end = moveTo' (cursorDown start buffer) buffer
    check (_, end)   = moveTo end buffer

moveUp :: TextBuffer -> TextBuffer
moveUp buffer = check selection
  where
    selection        = getSelection buffer
    check (start, end)
      | start == end = moveTo' (cursorUp start buffer) buffer
    check (start, _) = moveTo start buffer

cursorLeft :: Cursor -> TextBuffer -> Cursor
cursorLeft (Cursor 0 0) _buffer = Cursor 0 0
cursorLeft (Cursor l 0) buffer  = cursorToEndOfLine (l - 1) buffer
cursorLeft (Cursor l c) _buffer = Cursor l (c - 1)

moveWordLeft :: TextBuffer -> TextBuffer
moveWordLeft buffer = check selection
  where
    selection        = getSelection buffer
    check (start, end)
      | start == end = moveTo (cursorWordLeft start buffer) buffer
    check (start, _) = moveTo start buffer

cursorWordLeft :: Cursor -> TextBuffer -> Cursor
cursorWordLeft (Cursor 0 0) _buffer = Cursor 0 0
cursorWordLeft (Cursor l 0) buffer  = cursorToEndOfLine (l - 1) buffer
cursorWordLeft (Cursor l c) buffer = 
    let curLine     = Seq.index (bufText buffer) l
        spaces      = Seq.elemIndicesR ' ' curLine
        boundaryCol = 0
        nextSpaceL  = fromMaybe boundaryCol ((+1) <$> find (< (c-1)) spaces)
    in Cursor l nextSpaceL

cursorWordRight :: Cursor -> TextBuffer -> Cursor
cursorWordRight cursor@(Cursor l c) buffer
  | l == maxLine &&
    c == lineLength l text = cursor
  | c == lineLength l text = cursorToStartOfLine (l + 1)
  | otherwise = 
      let curLine     = Seq.index (bufText buffer) l
          spaces      = Seq.elemIndicesL ' ' curLine
          boundaryCol = lineLength l (bufText buffer)
          nextSpaceR  = fromMaybe boundaryCol (find (> c) spaces)
      in Cursor l nextSpaceR
  where
    maxLine = length text - 1
    text = bufText buffer

moveWordRight :: TextBuffer -> TextBuffer
moveWordRight buffer = check selection
  where
    selection       = getSelection buffer
    check (start, end)
      | start == end = moveTo (cursorWordRight start buffer) buffer
    check (_, end) = moveTo end buffer

cursorUp :: Cursor -> TextBuffer -> Cursor
cursorUp (Cursor 0 _) _buffer = Cursor 0 0
cursorUp (Cursor l _) buffer  = cursorToCurrentColumnInLine (l - 1) buffer

cursorDown :: Cursor -> TextBuffer -> Cursor
cursorDown cursor@(Cursor l _) buffer
  | l == maxLine = cursor
  | otherwise    = cursorToCurrentColumnInLine (l + 1) buffer
  where
    maxLine = length text - 1
    text = bufText buffer

cursorToCurrentColumnInLine :: LineNum -> TextBuffer -> Cursor
cursorToCurrentColumnInLine l buffer = cursorToColumnInLine l c buffer
    where c = bufColumn buffer

cursorToColumnInLine :: LineNum -> ColNum -> TextBuffer -> Cursor
cursorToColumnInLine l c buffer = Cursor l (min c lineLen)
  where
    lineLen = lineLength l (bufText buffer)

cursorToEndOfLine :: LineNum -> TextBuffer -> Cursor
cursorToEndOfLine l buffer = Cursor l lineLen
  where
    lineLen = lineLength l (bufText buffer)

selectUp :: TextBuffer -> TextBuffer
selectUp buffer = go selection
  where
    selection       = getSelection buffer
    go (start, end) = buffer { bufSelection = Just (cursorUp start buffer, end) }

selectDown :: TextBuffer -> TextBuffer
selectDown buffer = go selection
  where
    selection       = getSelection buffer
    go (start, end) = buffer { bufSelection = Just (start, cursorDown end buffer) }

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

setSelection :: Selection -> TextBuffer -> TextBuffer
setSelection selection buffer = 
    updateCurrentColumn 
        (buffer { bufSelection = Just selection })

selectWordLeft :: TextBuffer -> TextBuffer
selectWordLeft buffer = updateCurrentColumn (go selection)
  where
    selection       = getSelection buffer
    go (start, end) = buffer { bufSelection = Just (cursorWordLeft start buffer, end) }

selectWordRight :: TextBuffer -> TextBuffer
selectWordRight buffer = updateCurrentColumn (go selection)
  where
    selection       = getSelection buffer
    go (start, end) = buffer { bufSelection = Just (start, cursorWordRight end buffer) }

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

carriageReturn :: TextBuffer -> TextBuffer
carriageReturn buffer = 
    let indent = currentIndentation buffer
    in insertString ('\n' : replicate indent ' ') buffer
    
currentIndentation :: TextBuffer -> Int
currentIndentation buffer = lineIndentation l (bufText buffer)
    where (Cursor l _, _) = getSelection buffer

lineIndentation :: Int -> Seq (Seq Char) -> Int
lineIndentation l textSeq = Seq.length . Seq.takeWhileL (== ' ') $ line
    where line = Seq.index textSeq l

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
