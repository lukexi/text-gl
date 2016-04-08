{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.GL.TextBuffer.Render where

import Graphics.GL.Pal hiding (trace)
import Control.Lens.Extra

import Control.Monad.Reader

import Graphics.GL.Freetype.Types
import Graphics.GL.Freetype.API
import Graphics.GL.TextBuffer.Metrics
import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Types



createTextRenderer :: MonadIO m => Font -> TextBuffer -> m TextRenderer
createTextRenderer font textBuffer = do
    let shader = fntShader font
    glyphVAO <- newVAO

    -- Reserve space for 10000 characters
    glyphIndexBuffer  <- bufferData GL_DYNAMIC_DRAW ([0..10000] :: [GLint])
    glyphOffsetBuffer <- bufferData GL_DYNAMIC_DRAW (replicate 10000 (0::V4 GLfloat))

    withVAO glyphVAO $ do
        withArrayBuffer glyphIndexBuffer $ do
            let name = "aInstanceGlyphIndex"
            attribute <- getShaderAttribute shader name
            assignIntegerAttribute shader name GL_INT 1
            vertexAttribDivisor attribute 1
        withArrayBuffer glyphOffsetBuffer $ do
            let name = "aInstanceCharacterOffset"
            attribute <- getShaderAttribute shader name
            assignFloatAttribute shader name GL_FLOAT 4
            vertexAttribDivisor attribute 1

    updateMetrics $ TextRenderer
        { _txrTextBuffer         = textBuffer
        , _txrTextMetrics        = TextMetrics 
                                    { txmCharIndices = mempty
                                    , txmCharOffsets = mempty
                                    , txmNumChars = 0 }
        , _txrVAO                = glyphVAO
        , _txrCorrectionM44      = identity
        , _txrIndexBuffer        = glyphIndexBuffer
        , _txrOffsetBuffer       = glyphOffsetBuffer
        , _txrFont               = font
        , _txrDragRoot           = Nothing
        , _txrFileEventListener  = Nothing
        }

-- | Recalculates the character indices and glyph offsets of a TextBuffer 
-- and writes them into the TextRenderer's VertexBuffers
updateMetrics :: MonadIO m => TextRenderer -> m TextRenderer
updateMetrics textRenderer@TextRenderer{..} = do
    let textMetrics@TextMetrics{..} = calculateMetrics _txrTextBuffer _txrFont
    bufferSubData _txrIndexBuffer  txmCharIndices
    bufferSubData _txrOffsetBuffer (map snd txmCharOffsets)

    let newTextRenderer  = textRenderer { _txrTextMetrics = textMetrics }
        newCorrectionM44 = correctionMatrixForTextRenderer newTextRenderer
        newTextRenderer' = newTextRenderer { _txrCorrectionM44 = newCorrectionM44 }
    return newTextRenderer'

-- | This is quick and dirty.
-- We should keep the bounding boxes of the characters during
-- calculateIndicesAndOffsets, which we should cache in the TextBuffer.
-- We currently use the point size which is quite wrong
rayToTextRendererCursor :: Ray Float -> TextRenderer -> M44 Float -> (Maybe Cursor)
rayToTextRendererCursor ray textRenderer model44 = 
    let textModel44   = model44 !*! correctionMatrixForTextRenderer textRenderer
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
            hits = filter (\(_i, V4 x y _ _) -> 
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


correctionMatrixForTextRenderer :: TextRenderer -> M44 Float
correctionMatrixForTextRenderer textRenderer = 
            scaleMatrix resolutionCompensationScale 
        !*! translateMatrix centeringOffset
  where
    (dimX, dimY) = textSeqDimensions . bufText $ textRenderer ^. txrTextBuffer
    font = textRenderer ^. txrFont
    (numCharsX, numCharsY) = (realToFrac dimX, realToFrac dimY)
    -- Also scale by the width of a wide character
    charWidthFull = gmAdvanceX (glyMetrics (fntGlyphForChar font $ '_')) * numCharsX
    charHeightFull = fntPointSize font
    charHeight = 1 / charHeightFull
    --charWidth  = 1 / charWidthFull
    lineSpacingOffset = charHeightFull * 0.15
    centeringOffset = V3 (-charWidthFull/2) (charHeightFull * numCharsY/2 + lineSpacingOffset) 0
    -- Ensures the characters are always the same 
    -- size no matter what point size was specified
    resolutionCompensationScale = realToFrac charHeight

-- | RenderText renders at a scale where 1 GL Unit = 1 fullheight character.
-- All text is rendered centered based on the number of rows and the largest number
-- of columns. So to choose how many characters you want to be able to fit, you should
-- scale the text's Model matrix by 1/numChars.
renderText :: MonadIO m => TextRenderer -> M44 GLfloat -> V3 GLfloat -> m ()
renderText textRenderer mvp color = withSharedFont (textRenderer ^. txrFont) $ 
    renderTextOfSameFont textRenderer mvp color

-- | Lets us share the calls to useProgram etc. among a bunch of text renderings
withSharedFont :: MonadIO m => Font -> ReaderT Font m b -> m b
withSharedFont font@Font{..} renderActions = do
    let GlyphUniforms{..} = fntUniforms
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)
    uniformI   uTexture 0

    uniformF uTime =<< getNow

    runReaderT renderActions font 

renderTextOfSameFont :: (MonadIO m, MonadReader Font m) 
                     => TextRenderer -> M44 GLfloat -> V3 GLfloat -> m ()
renderTextOfSameFont textRenderer mvp color = do
    Font{..} <- ask
    let TextMetrics{..}   = textRenderer ^. txrTextMetrics
        GlyphUniforms{..} = fntUniforms
        rendererVAO       = textRenderer ^. txrVAO
        correctionM44     = textRenderer ^. txrCorrectionM44
    
    let correctedMVP      = mvp !*! correctionM44

    uniformM44 uMVP     correctedMVP
    uniformV3  uColor   color

    let numVertices  = 4
        numInstances = fromIntegral txmNumChars
    withVAO rendererVAO $ 
        glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()
