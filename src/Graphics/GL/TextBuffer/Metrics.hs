{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.Metrics where

import Graphics.GL.Pal hiding (trace)

import Graphics.GL.Freetype.API
import Graphics.GL.Freetype.Types
import Graphics.GL.TextBuffer.Types
import Graphics.GL.TextBuffer.TextBuffer
import qualified Data.Sequence as Seq

-- | Recalculates the character indices and glyph offsets of a TextBuffer
calculateMetrics :: TextBuffer -> Font -> TextMetrics
calculateMetrics TextBuffer{..} Font{..} = 
    let blockGlyph         = glyphForChar blockChar
        cursorGlyph        = glyphForChar cursorChar
        glyphForChar       = fntGlyphForChar
        pointSize          = fntPointSize
        renderLine (indicesL, offsetsL) lineNum line = 
            let charYOffset = -(fromIntegral lineNum) * pointSize
                renderChar (lastXOffset, maybeLastChar, indicesC, offsetsC) colNum character =
                      -- Render newlines as spaces
                    let glyph   = glyphForChar character
            
                        -- Find the optimal kerning between this character and 
                        -- the last one rendered (if any)
                        kerning = maybe 0 (getGlyphKerning glyph) maybeLastChar
            
                        -- Adjust the character's x offset to nestle against the previous character
                        charXOffset = lastXOffset + kerning
                        nextXOffset = charXOffset + gmAdvanceX (glyMetrics glyph)
                        charCursor = Cursor lineNum colNum
                        charOffset  = (charCursor, V2 charXOffset charYOffset)
                        (newIndicesC, newOffsetsC) = (glyIndex glyph : indicesC, 
                                                      charOffset     : offsetsC)
                        (newIndicesC', newOffsetsC') = case bufSelection of
                          Just selectionCursor
                            -- Add a cursor when line and col num match the (zero-width) cursor
                            | charCursor `cursorEqual` selectionCursor ->
                                (glyIndex cursorGlyph : newIndicesC, 
                                 charOffset           : newOffsetsC )
                            -- Add a selection block when within selection
                            | charCursor `cursorWithin` selectionCursor ->
                                ( glyIndex blockGlyph : newIndicesC
                                , charOffset          : newOffsetsC )
                          _ -> ( newIndicesC, newOffsetsC )
                    in (nextXOffset, Just character, newIndicesC', newOffsetsC')
                (xOffset, _, newIndicesL, newOffsetsL) = 
                    Seq.foldlWithIndex renderChar (0, Nothing, indicesL, offsetsL) line
                -- Make sure the cursor remains visible at the end of lines
                -- and in empty lines that are part of the selection
                (newIndicesL', newOffsetsL') = case bufSelection of
                    Just selection -> case getZeroWidth selection of
                        Just (Cursor l c)
                            | l == lineNum && c == length line -> 
                                ( glyIndex cursorGlyph : newIndicesL
                                , (Cursor lineNum c, V2 xOffset charYOffset) : newOffsetsL )
                        Nothing
                            | length line == 0 && Cursor lineNum 0 `cursorWithin` selection ->
                                ( glyIndex blockGlyph : newIndicesL
                                , (Cursor lineNum 0, V2 xOffset charYOffset) : newOffsetsL )
                        _ -> (newIndicesL, newOffsetsL)
                    _ -> (newIndicesL, newOffsetsL)
            in (newIndicesL', newOffsetsL')
        (charIndices, charOffsets) = Seq.foldlWithIndex renderLine ([], []) bufText
    in TextMetrics
        { txmCharIndices = reverse charIndices
        , txmCharOffsets = reverse charOffsets
        , txmNumChars    = length charIndices
        }

