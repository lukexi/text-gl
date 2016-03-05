{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Graphics.GL.TextBuffer.Render where

import Data.Foldable

import Graphics.GL.Pal hiding (trace)
import Control.Lens.Extra

import Control.Monad.Trans
import Control.Monad

import Graphics.GL.Freetype.Types
import Graphics.GL.Freetype.Font
import Graphics.GL.TextBuffer.Metrics
import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Types

createTextRenderer :: MonadIO m => Font -> TextBuffer -> m TextRenderer
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
        , _txrDragRoot           = Nothing
        }

-- | Recalculates the character indices and glyph offsets of a TextBuffer 
-- and writes them into the TextRenderer's VertexBuffers
updateMetrics :: MonadIO m => TextRenderer -> m TextRenderer
updateMetrics textRenderer@TextRenderer{..} = do
    let textMetrics@TextMetrics{..} = calculateMetrics _txrTextBuffer _txrFont
    bufferSubData _txrIndexBuffer  txmCharIndices
    bufferSubData _txrOffsetBuffer (concatMap (toList . snd) txmCharOffsets)
    return textRenderer { _txrTextMetrics = textMetrics }

-- | This is quick and dirty.
-- We should keep the bounding boxes of the characters during
-- calculateIndicesAndOffsets, which we should cache in the TextBuffer.
-- We currently use the point size which is quite wrong
rayToTextRendererCursor :: Ray Float -> TextRenderer -> M44 Float -> (Maybe Cursor)
rayToTextRendererCursor ray textRenderer model44 = 
    let textModel44   = model44 !*! correctionMatrixForFont font
        font          = textRenderer ^. txrFont
        aabb          = (0, V3 1 (-1) 0) -- Is this right??
        mIntersection = rayOBBIntersection ray aabb textModel44
    in join . flip fmap mIntersection $ \intersectionDistance -> 
        let worldPoint       = projectRay ray intersectionDistance
            V3 cursX cursY _ = worldPointToModelPoint textModel44 worldPoint
            -- The width here is a guess; should get this from GlyphMetrics
            -- during updateIndicesAndOffsets
            (charW, charH)   = (fntPointSize font * 0.66, fntPointSize font)
            charOffsets      = txmCharOffsets (textRenderer ^. txrTextMetrics)
            hits = filter (\(_i, V2 x y) -> 
                           cursX > x 
                        && cursX < (x + charW) 
                        && cursY > y 
                        && cursY < (y + charH)) 
                    charOffsets
        in case hits of
            ((i, _):_) -> Just i
            []         -> Nothing

setCursorTextRendererWithRay :: MonadIO m => Ray GLfloat -> TextRenderer -> M44 GLfloat -> m TextRenderer
setCursorTextRendererWithRay ray textRenderer model44 = 
    case rayToTextRendererCursor ray textRenderer model44 of
        Just i  -> updateMetrics (textRenderer & txrTextBuffer %~ moveTo i)
        Nothing -> return textRenderer

beginDrag :: MonadIO m => Cursor -> TextRenderer -> m TextRenderer
beginDrag cursor textRenderer = 
    updateMetrics (textRenderer & txrTextBuffer %~ moveTo cursor
                                & txrDragRoot ?~ cursor)

continueDrag :: MonadIO m => Cursor -> TextRenderer -> m TextRenderer
continueDrag cursor textRenderer = case textRenderer ^. txrDragRoot of
    Nothing -> return textRenderer
    Just dragRoot -> updateMetrics (textRenderer & txrTextBuffer %~ setSelection newSel)
        where newSel = if cursor > dragRoot then (dragRoot, cursor) else (cursor, dragRoot)

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
        numInstances = fromIntegral txmNumChars
    withVAO rendererVAO $ 
      glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()
