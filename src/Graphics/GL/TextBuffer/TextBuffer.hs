{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Graphics.GL.TextBuffer.TextBuffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>), (<|))
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
cursorEqual cursor (selectionStart, selectionEnd) =
    cursor == selectionStart && cursor == selectionEnd

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

moveSeqRangeLeft :: Int -> Int -> Seq a -> Maybe (Seq a)
moveSeqRangeLeft startIndex endIndex aSeq =
    let (begin, middle, end) = divideSeq startIndex endIndex aSeq
    in case Seq.viewr begin of
        EmptyR   -> Nothing
        newBegin :> moved -> Just (newBegin <> middle <> (moved <| end))

moveSeqRangeRight :: Int -> Int -> Seq a -> Maybe (Seq a)
moveSeqRangeRight startIndex endIndex aSeq =
    let (begin, middle, end) = divideSeq startIndex endIndex aSeq
    in case Seq.viewl end of
        EmptyL          -> Nothing
        moved :< newEnd -> Just ((begin |> moved) <> middle <> newEnd)

divideSeq :: Int -> Int -> Seq a -> (Seq a, Seq a, Seq a)
divideSeq startIndex endIndex aSeq = (begin, middle, end)
  where
    (begin, rest) = Seq.splitAt startIndex aSeq
    (middle, end) = Seq.splitAt (endIndex - startIndex) rest

seqDuplicateIndex :: Int -> Seq a -> Seq a
seqDuplicateIndex index aSeq = before <> Seq.take 1 after <> after
  where
    (before, after) = Seq.splitAt index aSeq

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
    , bufDims      = (0,0)
    }

isEmptyTextBuffer :: TextBuffer -> Bool
isEmptyTextBuffer TextBuffer{..} = Seq.null bufText

textBufferWithPath :: FilePath -> String -> TextBuffer
textBufferWithPath filePath string = (textBufferFromString string)
    { bufPath = Just filePath
    }

textBufferFromString :: String -> TextBuffer
textBufferFromString string = newTextBuffer
    { bufText = textSeq
    , bufDims = textSeqDimensions textSeq
    }
    where textSeq = textSeqFromString string

setTextFromString :: String -> TextBuffer -> TextBuffer
setTextFromString string buffer =
    pushUndo . validateSelection $
        buffer
            { bufText = textSeq
            , bufDims = textSeqDimensions textSeq
            }
    where textSeq = textSeqFromString string


validateSelection :: TextBuffer -> TextBuffer
validateSelection textBuffer@TextBuffer{..} = case bufSelection of
    Nothing -> textBuffer
    Just (cursorBegin, cursorEnd) -> updateCurrentColumn $ textBuffer
        { bufSelection = Just ( validateCursor cursorBegin textBuffer
                              , validateCursor cursorEnd   textBuffer )
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
selectionFromTextBuffer TextBuffer{..} = fromMaybe "" $
    selectionFromTextSeq bufText <$> bufSelection

currentColumn :: TextBuffer -> Int
currentColumn buffer@TextBuffer{..} = startColNum
    where (Cursor _ startColNum, _) = getSelection buffer


-- FIXME Disabling Undo while I verify that it
-- doesn't cause too much memory pressure implemented this way
pushUndo :: TextBuffer -> TextBuffer
--pushUndo buffer = buffer { bufUndo = Just buffer }
pushUndo = id

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
    , bufDims      = textSeqDimensions newText
    }
    where
        newBuffer  = pushUndo buffer
        newText    = insertTextSeq (getSelection buffer) textSeq (bufText buffer)
        -- Calculate where the cursor should be after inserting the text
        newCursor  = Cursor newLineNum newColNum
        newLineNum = startLineNum + (length textSeq - 1)
        newColNum  = if newLineNum == startLineNum then startColNum + lastLen else lastLen
        lastLen    = fromMaybe 0 (length <$> seqLast textSeq)
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
    check (_, end)   = moveTo end buffer

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
selectUp buffer = setSelectionVertical (cursorUp start buffer, end) buffer
  where
    (start, end) = getSelection buffer

selectDown :: TextBuffer -> TextBuffer
selectDown buffer = setSelectionVertical (start, cursorDown end buffer) buffer
  where
    (start, end) = getSelection buffer

selectLeft :: TextBuffer -> TextBuffer
selectLeft buffer = setSelection (cursorLeft start buffer, end) buffer
  where
    (start, end)  = getSelection buffer

selectRight :: TextBuffer -> TextBuffer
selectRight buffer = setSelection (start, cursorRight end buffer) buffer
  where
    (start, end) = getSelection buffer

-- | We don't want to change the current column when moving up and down
-- past lines that may be shorter than our current column.
setSelectionVertical :: Selection -> TextBuffer -> TextBuffer
setSelectionVertical selection buffer = buffer { bufSelection = Just selection }

setSelection :: Selection -> TextBuffer -> TextBuffer
setSelection selection buffer =
    updateCurrentColumn
        (buffer { bufSelection = Just selection })

selectWordLeft :: TextBuffer -> TextBuffer
selectWordLeft buffer = setSelection (cursorWordLeft start buffer, end) buffer
  where
    (start, end) = getSelection buffer

selectWordRight :: TextBuffer -> TextBuffer
selectWordRight buffer = setSelection (start, cursorWordRight end buffer) buffer
  where
    (start, end) = getSelection buffer

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

carriageReturnToNextLine :: TextBuffer -> TextBuffer
carriageReturnToNextLine buffer = carriageReturn . moveToEndOfLine endLine $ buffer
    where
        (Cursor _ _, Cursor endLine _) = getSelection buffer

overSelectedLines :: (LineNum -> TextBuffer -> TextBuffer)
                              -> TextBuffer -> TextBuffer
overSelectedLines action buffer = foldl' (flip action) buffer [startLine..endLine]
    where
        (Cursor startLine _, Cursor endLine _) = getSelection buffer

indentLines :: TextBuffer -> TextBuffer
indentLines   = overSelectedLines indentLine

unindentLines :: TextBuffer -> TextBuffer
unindentLines = overSelectedLines unindentLine

toggleLinesComment :: TextBuffer -> TextBuffer
toggleLinesComment = overSelectedLines toggleLineComment

defaultIndent :: Int
defaultIndent = 4

indentString :: String
indentString = replicate defaultIndent ' '

commentString :: String
commentString = "--"

toggleLineComment :: LineNum -> TextBuffer -> TextBuffer
toggleLineComment l buffer =
    let commentLen         = length commentString
        line               = getLineAt l (bufText buffer)
        isCommented        = toList (Seq.take commentLen line) == commentString
        (moved, newBuffer) = if isCommented
            then (-commentLen, backspace . setSelection (Cursor l 0, Cursor l commentLen) $ buffer)
            else ( commentLen, insertString commentString . moveToStartOfLine l $ buffer)
        fixedSelection = selectionMovedByCols moved l (getSelection buffer)
    in setSelection fixedSelection newBuffer


indentLine :: LineNum -> TextBuffer -> TextBuffer
indentLine l buffer =
    let newBuffer      = insertString indentString . moveToStartOfLine l $ buffer
        fixedSelection = selectionMovedByCols (length indentString) l (getSelection buffer)
    in setSelection fixedSelection newBuffer

unindentLine :: LineNum -> TextBuffer -> TextBuffer
unindentLine l buffer =
    let spacesToDelete = min defaultIndent . indentationOfLine l . bufText $ buffer
        newBuffer      = backspace . setSelection (Cursor l 0, Cursor l spacesToDelete) $ buffer
        -- Get the selection (from the original buffer since we modify it to implement deletion)
        -- and move it back by whatever we deleted
        fixedSelection = selectionMovedByCols (-spacesToDelete) l (getSelection buffer)
    in if spacesToDelete > 0
        then setSelection fixedSelection newBuffer
        else buffer

-- | Check if the given line matches the start or end line of the selection,
-- and if so, move the column of that line by the given number of colums.
selectionMovedByCols :: ColNum -> LineNum -> (Cursor, Cursor) -> (Cursor, Cursor)
selectionMovedByCols cols l selection =
    let (Cursor startLine startCol, Cursor endLine endCol) = selection
        newSelection
            | l == startLine && l == endLine = ( Cursor startLine (startCol + cols)
                                               , Cursor endLine (endCol + cols))
            | l == startLine = (Cursor startLine (startCol + cols), Cursor endLine endCol)
            | l == endLine = (Cursor startLine startCol, Cursor endLine (endCol + cols))
            | otherwise = selection
    in newSelection

moveLinesDown :: TextBuffer -> TextBuffer
moveLinesDown = moveLinesWith moveSeqRangeRight 1
moveLinesUp   :: TextBuffer -> TextBuffer
moveLinesUp   = moveLinesWith moveSeqRangeLeft (-1)

moveLinesWith :: (LineNum -> LineNum -> TextSeq -> Maybe TextSeq)
              -> LineNum -> TextBuffer -> TextBuffer
moveLinesWith moveFunc numLinesMoved buffer =
    let (Cursor startLine startCol, Cursor endLine endCol) = getSelection buffer
    in case moveFunc startLine (endLine + 1) (bufText buffer) of
        Nothing -> buffer
        Just newSeq -> (pushUndo buffer) { bufText = newSeq, bufSelection = Just newSelection }
            where newSelection = ( Cursor (startLine + numLinesMoved) startCol
                                 , Cursor (endLine + numLinesMoved) endCol
                                 )
-- | Acts on the last line of the selection,
-- and moves the cursor to the new line
duplicateLine :: TextBuffer -> TextBuffer
duplicateLine buffer =
    let (Cursor startLine startCol, Cursor endLine endCol) = getSelection buffer
        newSeq       = seqDuplicateIndex endLine (bufText buffer)
        newSelection = (Cursor (endLine + 1) endCol, Cursor (endLine + 1) endCol)
    in (pushUndo buffer) { bufText = newSeq, bufSelection = Just newSelection}

currentIndentation :: TextBuffer -> Int
currentIndentation buffer = indentationOfLine l (bufText buffer)
    where (Cursor l _, _) = getSelection buffer

indentationOfLine :: Int -> Seq (Seq Char) -> Int
indentationOfLine l = Seq.length . Seq.takeWhileL (== ' ') . getLineAt l

getLineAt :: Int -> Seq (Seq Char) -> Seq Char
getLineAt l textSeq = Seq.index textSeq l

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
