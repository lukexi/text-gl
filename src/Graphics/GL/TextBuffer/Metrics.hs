{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.Metrics where

import Graphics.GL.Pal hiding (trace)

import Graphics.GL.Freetype.Types
import Graphics.GL.TextBuffer.Types
import Graphics.GL.Freetype.API
import qualified Data.Sequence as Seq
import Graphics.GL.TextBuffer.TextBuffer

-- | Recalculates the character indices and glyph offsets of a TextBuffer
calculateMetrics :: TextBuffer -> Font -> TextMetrics
calculateMetrics TextBuffer{..} Font{..} = 
    let blockGlyph         = glyphForChar blockChar
        cursorGlyph        = glyphForChar cursorChar
        glyphForChar       = fntGlyphForChar
        pointSize          = fntPointSize
        renderLine (indicesL, offsetsL) lineNum line = 
            let renderChar (lastXOffset, maybeLastChar, indicesC, offsetsC) colNum character =
                      -- Render newlines as spaces
                    let glyph   = glyphForChar (if character == '\n' then ' ' else character)
            
                        -- Find the optimal kerning between this character and the last one rendered (if any)
                        kerning = maybe 0 (getGlyphKerning glyph) maybeLastChar
            
                        -- Adjust the character's x offset to nestle against the previous character
                        charYOffset = -(fromIntegral lineNum) * pointSize
                        charXOffset = lastXOffset + kerning
                        nextXOffset = charXOffset + gmAdvanceX (glyMetrics glyph)
                        charCursor = Cursor lineNum colNum
                        charOffset  = (charCursor, V2 charXOffset charYOffset)
                        (newIndicesC, newOffsetsC) = case bufSelection of
                          Just selectionCursor
                            -- Add a cursor when line and col num match the (zero-width) cursor
                            | charCursor `cursorEqual` selectionCursor ->
                              let indices' = glyIndex cursorGlyph : glyIndex glyph : indicesC
                                  offsets' = charOffset           : charOffset     : offsetsC
                              in (indices', offsets')
                            -- Add a selection block when within selection
                            | charCursor `cursorWithin` selectionCursor ->
                              let indices' = glyIndex blockGlyph : glyIndex glyph : indicesC
                                  offsets' = charOffset          : charOffset     : offsetsC
                              in (indices', offsets')
                          _ ->
                            let indices' = glyIndex glyph : indicesC
                                offsets' = charOffset     : offsetsC
                            in (indices', offsets')
                    in (nextXOffset, Just character, newIndicesC, newOffsetsC)
                (_, _, newIndicesL, newOffsetsL) = Seq.foldlWithIndex renderChar (0, Nothing, indicesL, offsetsL) line
            in (newIndicesL, newOffsetsL)
        (charIndices, charOffsets) = Seq.foldlWithIndex renderLine ([], []) bufText
    in TextMetrics
        { txmCharIndices = reverse charIndices
        , txmCharOffsets = reverse charOffsets
        , txmNumChars    = length charIndices
        }


  
