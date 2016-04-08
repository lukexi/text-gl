{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.Metrics where

import Graphics.GL.Pal hiding (trace)

import Graphics.GL.Freetype.API
import Graphics.GL.Freetype.Types
import Graphics.GL.TextBuffer.Types
import Graphics.GL.TextBuffer.TextBuffer
import qualified Data.Sequence as Seq

selectionZOffset :: Float
selectionZOffset = -0.01

cursorZOffset :: Float
cursorZOffset = 0.01

cursorColorMode :: Float
cursorColorMode    = -1

selectionColorMode :: Float
selectionColorMode = 1

-- | Recalculates the character indices and glyph offsets of a TextBuffer
calculateMetrics :: TextBuffer -> Font -> TextMetrics
calculateMetrics textBuffer@TextBuffer{..} Font{..} = 
    let blockGlyph         = glyphForChar blockChar
        cursorGlyph        = glyphForChar cursorChar
        glyphForChar       = fntGlyphForChar
        pointSize          = fntPointSize
        renderLine (indicesL, offsetsL) lineNum line = 
            let charYOffset = -(fromIntegral lineNum + 1) * pointSize
                
                (xOffset, _, newIndicesL, newOffsetsL) = 
                    Seq.foldlWithIndex 
                        (renderChar textBuffer charYOffset glyphForChar blockGlyph cursorGlyph lineNum) 
                        (0, Nothing, indicesL, offsetsL) 
                        line

                -- Make sure the cursor remains visible at the end of lines
                -- and in empty lines that are part of the selection

                -- FIXME: I think this can be simplified by adding a ' ' character to the 'line' variable above. 
                (newIndicesL', newOffsetsL') = case bufSelection of
                    Just selection -> case getZeroWidth selection of
                        Just (Cursor l c)
                            | l == lineNum && c == length line -> 
                                ( glyIndex cursorGlyph : newIndicesL
                                , (Cursor lineNum c, V4 xOffset charYOffset cursorZOffset cursorColorMode) : newOffsetsL )
                        Nothing
                            | length line == 0 && Cursor lineNum 0 `cursorWithin` selection ->
                                ( glyIndex blockGlyph : newIndicesL
                                , (Cursor lineNum 0, V4 xOffset charYOffset selectionZOffset selectionColorMode ) : newOffsetsL )
                        _ -> (newIndicesL, newOffsetsL)

                    _ -> (newIndicesL, newOffsetsL)
            in (newIndicesL', newOffsetsL')
        (charIndices, charOffsets) = Seq.foldlWithIndex renderLine ([], []) bufText
    in TextMetrics
        { txmCharIndices = reverse charIndices
        , txmCharOffsets = reverse charOffsets
        , txmNumChars    = length charIndices
        }


renderChar :: TextBuffer
           -> Float
           -> (Char -> Glyph)
           -> Glyph
           -> Glyph
           -> LineNum
           -> (Float, Maybe Char, [GLint], [(Cursor, V4 Float)])
           -> ColNum
           -> Char
           -> (Float, Maybe Char, [GLint], [(Cursor, V4 Float)])
renderChar TextBuffer{..} charYOffset glyphForChar blockGlyph cursorGlyph lineNum (lastXOffset, maybeLastChar, indicesC, offsetsC) colNum character =
      -- Render newlines as spaces
    let glyph   = glyphForChar character

        -- Find the optimal kerning between this character and 
        -- the last one rendered (if any)
        kerning = maybe 0 (getGlyphKerning glyph) maybeLastChar

        -- Adjust the character's x offset to nestle against the previous character
        charXOffset = lastXOffset + kerning
        nextXOffset = charXOffset + gmAdvanceX (glyMetrics glyph)
        charCursor = Cursor lineNum colNum
        charOffset  = (charCursor, V4 charXOffset charYOffset 0 0)

        -- Add the actual character,
        (newIndicesC, newOffsetsC) = (glyIndex glyph : indicesC, 
                                      charOffset     : offsetsC)

        -- And also add a cursor if 
        (newIndicesC', newOffsetsC') = case bufSelection of
          Just selectionCursor
            -- Add a cursor when line and col num match the (zero-width) cursor
            | charCursor `cursorEqual` selectionCursor ->
                ( glyIndex cursorGlyph                                     : newIndicesC
                , (charCursor, V4 charXOffset charYOffset cursorZOffset cursorColorMode) : newOffsetsC )
            -- Add a selection block when within selection
            | charCursor `cursorWithin` selectionCursor ->
                ( glyIndex blockGlyph                                            : newIndicesC
                , (charCursor, V4 charXOffset charYOffset selectionZOffset selectionColorMode) : newOffsetsC )
          _ -> ( newIndicesC, newOffsetsC )
    in (nextXOffset, Just character, newIndicesC', newOffsetsC')
