{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Freetype.TextRenderer where

import Data.Foldable

import Graphics.GL.Pal hiding (trace)
import Control.Lens

import Control.Monad.Trans

import Graphics.GL.Freetype.Types
import Graphics.GL.Freetype.Font
import Graphics.GL.Freetype.TextMetrics
import Graphics.GL.Freetype.TextBuffer

createTextRenderer :: Font -> TextBuffer -> IO TextRenderer
createTextRenderer font textBuffer = do
    let shader = fntShader font
    glyphVAO <- newVAO

    -- Reserve space for 10000 characters
    glyphIndexBuffer  <- bufferData GL_DYNAMIC_DRAW ([0..10000] :: [GLint])
    glyphOffsetBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList (replicate 10000 (0::V2 GLfloat)))

    withVAO glyphVAO $ do
      withArrayBuffer glyphIndexBuffer $ do
        let name = "aInstanceGlyphIndex"
        attribute <- getShaderAttribute shader name
        assignIntegerAttribute shader name GL_INT 1
        vertexAttribDivisor attribute 1
      withArrayBuffer glyphOffsetBuffer $ do
        let name = "aInstanceCharacterOffset"
        attribute <- getShaderAttribute shader name
        assignFloatAttribute shader name GL_FLOAT 2
        vertexAttribDivisor attribute 1

    updateMetrics $ TextRenderer
        { _txrTextBuffer         = textBuffer
        , _txrTextMetrics        = TextMetrics 
                                    { txmCharIndices = mempty
                                    , txmCharOffsets = mempty
                                    , txmNumChars = 0 }
        , _txrVAO                = glyphVAO
        , _txrIndexBuffer        = glyphIndexBuffer
        , _txrOffsetBuffer       = glyphOffsetBuffer
        , _txrFont               = font
        }
        

-- | Recalculates the character indices and glyph offsets of a TextBuffer 
-- and writes them into the TextRenderer's VertexBuffers
updateMetrics :: MonadIO m => TextRenderer -> m TextRenderer
updateMetrics textRenderer@TextRenderer{..} = do
    let textMetrics@TextMetrics{..} = calculateMetrics _txrTextBuffer _txrFont
    bufferSubData _txrIndexBuffer  txmCharIndices
    bufferSubData _txrOffsetBuffer (concatMap toList txmCharOffsets)
    return textRenderer { _txrTextMetrics = textMetrics }

-- | This is quick and dirty.
-- We should keep the bounding boxes of the characters during
-- calculateIndicesAndOffsets, which we should cache in the TextBuffer.
-- We currently use the point side which is quite wrong

castRayToBuffer :: MonadIO m => Ray Float -> TextRenderer -> M44 Float -> m TextRenderer
castRayToBuffer ray textRenderer model44 = do
    let textModel44  = model44 !*! correctionMatrixForFont font
        font         = textRenderer ^. txrFont
        aabb         = (0, V3 1 (-1) 0) -- Is this right??
        rayDir       = directionFromRay ray
        intersection = rayOBBIntersection ray aabb textModel44
  
    case intersection of
        Nothing -> return textRenderer
        Just intersectionDistance -> do
            let worldPoint = rayDir ^* intersectionDistance
                modelPoint = worldPointToModelPoint textModel44 worldPoint
                -- The width here is a guess; should get this from GlyphMetrics
                -- during updateIndicesAndOffsets
                (charW, charH) = (fntPointSize font * 0.66, fntPointSize font)

                charOffsets = txmCharOffsets (textRenderer ^. txrTextMetrics)
                (cursX, cursY) = (modelPoint ^. _x, modelPoint ^. _y)
                hits = filter (\(_i, V2 x y) -> 
                               cursX > x 
                            && cursX < (x + charW) 
                            && cursY > y 
                            && cursY < (y + charH)) (zip [0..] charOffsets)
                
            case hits of
              [] -> return textRenderer
              ((i, _):_) -> 
                updateMetrics (textRenderer & txrTextBuffer %~ moveCursorTo i)

renderText :: (MonadIO m) 
           => TextRenderer -> M44 GLfloat -> V3 GLfloat -> m ()
renderText textRenderer mvp color = do
    let font@Font{..}     = textRenderer ^. txrFont
        TextMetrics{..}   = textRenderer ^. txrTextMetrics
        GlyphUniforms{..} = fntUniforms
        rendererVAO       = textRenderer ^. txrVAO
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)

    let correctedMVP      = mvp !*! correctionMatrixForFont font

    uniformM44 uMVP     correctedMVP
    uniformI   uTexture 0
    uniformV3  uColor   color

    let numVertices  = 4
        -- Add 1 to ensure we still render the cursor
        numInstances = fromIntegral txmNumChars
    withVAO rendererVAO $ 
      glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()
