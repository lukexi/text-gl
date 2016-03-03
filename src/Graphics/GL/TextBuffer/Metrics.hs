{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.TextBuffer.Metrics where


import Data.Foldable

import Graphics.GL.Pal hiding (trace)


import Graphics.GL.Freetype.Types
import Graphics.GL.TextBuffer.Types
import Graphics.GL.Freetype.API

-- | Recalculates the character indices and glyph offsets of a TextBuffer
calculateMetrics :: TextBuffer -> Font -> TextMetrics
calculateMetrics TextBuffer{..} Font{..} = 
  let blockGlyph         = glyphForChar blockChar
      cursorGlyph        = glyphForChar cursorChar
      glyphForChar       = fntGlyphForChar
      pointSize          = fntPointSize
      renderChar (charNum, lineNum, lastXOffset, maybeLastChar, indicesF, offsetsF) character = 
            -- Render newlines as spaces
        let glyph   = glyphForChar (if character == '\n' then ' ' else character)

            -- Find the optimal kerning between this character and the last one rendered (if any)
            kerning = maybe 0 (getGlyphKerning glyph) maybeLastChar

            -- Adjust the character's x offset to nestle against the previous character
            charYOffset = -lineNum * pointSize
            charXOffset = lastXOffset + kerning
            nextXOffset = charXOffset + gmAdvanceX (glyMetrics glyph)
            charOffset  = V2 charXOffset charYOffset
            (newIndices, newOffsets) = case bufSelection of
              Just (selStart, selEnd)
                -- Add a cursor when charNum==selStart==selEnd
                | charNum == selStart && charNum == selEnd ->
                  let indices' = glyIndex cursorGlyph : glyIndex glyph : indicesF :: [GLint]
                      offsets' = charOffset           : charOffset     : offsetsF :: [V2 GLfloat]
                  in (indices', offsets')
                -- Add a selection block selStart<=charNum<selEnd
                | charNum >= selStart && charNum < selEnd ->
                  let indices' = glyIndex blockGlyph : glyIndex glyph : indicesF :: [GLint]
                      offsets' = charOffset          : charOffset     : offsetsF :: [V2 GLfloat]
                  in (indices', offsets')
              _ ->
                let indices' = glyIndex glyph : indicesF :: [GLint]
                    offsets' = charOffset     : offsetsF :: [V2 GLfloat]
                in (indices', offsets')
        
        in if character == '\n'
            then (charNum + 1, lineNum + 1,           0, Nothing       , newIndices, newOffsets)
            else (charNum + 1, lineNum    , nextXOffset, Just character, newIndices, newOffsets)
      (_, _, _, _, charIndices, charOffsets) = foldl' renderChar (0, 1, 0, Nothing, [], []) bufText
  in TextMetrics
    { txmCharIndices = reverse charIndices
    , txmCharOffsets = reverse charOffsets
    , txmNumChars    = length charIndices
    }


