{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.GL.TextBuffer.Render where

import Graphics.GL.Pal hiding (trace)
import Control.Lens.Extra

import Control.Monad.Reader
import Control.Concurrent.STM

import Graphics.GL.Freetype.Types
import Graphics.GL.Freetype.API
import Graphics.GL.TextBuffer.Metrics
import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Types

import Debug.Trace

createTextRenderer :: MonadIO m => Font -> TextBuffer -> m TextRenderer
createTextRenderer font textBuffer = do
    let shader = fntShader font
    glyphVAO <- newVAO

    -- Reserve space for 20000 characters
    let maxChars :: Num a => a
        maxChars = 20000
    glyphIndexBuffer  <- bufferData GL_DYNAMIC_DRAW ([0..maxChars] :: [GLint])
    glyphOffsetBuffer <- bufferData GL_DYNAMIC_DRAW (replicate maxChars (0::V4 GLfloat))

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

    needsBufferUploadVar <- liftIO $ newTVarIO True
    updateMetrics $ TextRenderer
        { _txrTextBuffer           = textBuffer
        , _txrTextMetrics          = TextMetrics
                                          { txmCharIndices = mempty
                                          , txmCharOffsets = mempty
                                          , txmNumChars = 0
                                          }
        , _txrVAO                  = glyphVAO
        , _txrCorrectionM44        = identity
        , _txrNeedsBufferUploadVar = needsBufferUploadVar
        , _txrIndexBuffer          = glyphIndexBuffer
        , _txrOffsetBuffer         = glyphOffsetBuffer
        , _txrFont                 = font
        , _txrDragRoot             = Nothing
        , _txrFileEventListener    = Nothing
        , _txrScroll               = V2 0 0
        , _txrScreenSize           = Nothing
        }

-- | Recalculates the character indices and glyph offsets of a TextBuffer
-- and writes them into the TextRenderer's ArrayBuffers
updateMetrics :: MonadIO m => TextRenderer -> m TextRenderer
updateMetrics textRenderer@TextRenderer{..} = do
    let newTextRenderer = textRenderer &~ do
            id %= updateScroll
            txrTextMetrics   .= calculateMetrics (textRenderer ^. txrTextBuffer) (textRenderer ^. txrFont)
            txrCorrectionM44 <~ (correctionMatrixForTextRenderer <$> use id)
        newTextMetrics = newTextRenderer ^. txrTextMetrics
    liftIO . atomically . writeTVar (newTextRenderer ^. txrNeedsBufferUploadVar) $ True

    return newTextRenderer

uploadCharacters textRenderer = do
    let textMetrics = textRenderer ^. txrTextMetrics
    bufferSubData (textRenderer ^. txrIndexBuffer)  (txmCharIndices textMetrics)
    bufferSubData (textRenderer ^. txrOffsetBuffer) (map snd (txmCharOffsets textMetrics))

updateScroll :: TextRenderer -> TextRenderer
updateScroll textRenderer = case (textRenderer ^. txrScreenSize, textRenderer ^. txrTextBuffer . to getSelection) of
    (Just screenSize, (Cursor (fromIntegral -> cursorLine) (fromIntegral -> cursorCol), _)) ->
        textRenderer &~ do
            V2 scrollX scrollY <- use txrScroll
            let V2 fontWidth fontHeight = fontDimensions (textRenderer ^. txrFont)
                V2 screenW screenHOrig  = fromIntegral <$> screenSize
                screenH                 = screenHOrig * (fontWidth / fontHeight)
                scrollPad               = 2
            -- Check for scrolling off the right of the screen
            when (cursorCol > screenW + scrollX - scrollPad) $
                txrScroll . _x .= cursorCol - (screenW - scrollPad)
            -- Check for scrolling off the bottom of the screen
            when (cursorLine > screenH + scrollY - scrollPad) $
                txrScroll . _y .= cursorLine - (screenH - scrollPad)
            -- Check for scrolling off the left of the screen
            when (cursorCol < scrollX + scrollPad) $
                txrScroll . _x .= cursorCol - scrollPad
            -- Check for scrolling off the top of the screen
            when (cursorLine < scrollY + scrollPad) $
                txrScroll . _y .= cursorLine - scrollPad

    _ -> textRenderer


correctionMatrixForTextRenderer :: TextRenderer -> M44 Float
correctionMatrixForTextRenderer textRenderer =
            scaleMatrix (realToFrac finalResScale)
        !*! translateMatrix (V3 (centeringOffset ^. _x) (centeringOffset ^. _y) 0)
  where
    textBuffer = textRenderer ^. txrTextBuffer
    (realToFrac -> numCharsX, realToFrac -> numCharsY) = textSeqDimensions (bufText textBuffer)
    fontDims@(V2 fontWidth fontHeight)                 = fontDimensions (textRenderer ^. txrFont)
    lineSpacing                                        = fontHeight * 0.15 -- 15% default line spacing

    (centeringOffset, finalResScale) = case textRenderer ^. txrScreenSize of
        Just screenSize -> (offset, scaling)
          where
            V2 screenW screenHOrig = fromIntegral <$> screenSize
            screenH = screenHOrig * (fontWidth / fontHeight)
            baseOffset = V2
                (-screenW / 2)
                ( screenH / 2)
            scaling = 1 / fontWidth / screenW
            offset = (baseOffset + scroll) * fontDims
            scroll = textRenderer ^. txrScroll & _x %~ negate

        -- If no explicit target screensize, fall back to centering based on height and width of text
        Nothing -> (offset, scaling)
          where
            longestLineWidth = fontWidth  * numCharsX
            totalLinesHeight = fontHeight * numCharsY
            offset = V2
                (-longestLineWidth/2)
                (totalLinesHeight/2 + lineSpacing)
            scaling = 1 / fontHeight


fontDimensions :: Font -> V2 Float
fontDimensions Font{..} = V2 charWidth charHeight
    where
        charWidth  = gmAdvanceX . glyMetrics . fntGlyphForChar $ '_'
        charHeight = fntPointSize

-- | RenderText renders at a scale where 1 GL Unit = 1 fullheight character.
-- All text is rendered centered based on the number of rows and the largest number
-- of columns. So to choose how many characters you want to be able to fit, you should
-- scale the text's Model matrix by 1/numChars.
renderText :: MonadIO m => TextRenderer -> M44 GLfloat -> M44 GLfloat  -> m ()
renderText textRenderer projViewM44 modelM44 =
    renderTextPreCorrected textRenderer projViewM44
        (modelM44 !*! textRenderer ^. txrCorrectionM44)

renderTextPreCorrected :: MonadIO m => TextRenderer -> M44 GLfloat -> M44 GLfloat -> m ()
renderTextPreCorrected textRenderer projViewM44 modelM44 =
    withSharedFont (textRenderer ^. txrFont) projViewM44 $
        renderTextPreCorrectedOfSameFont textRenderer modelM44

-- | Lets us share the calls to useProgram etc. among a bunch of text renderings
withSharedFont :: MonadIO m => Font -> M44 GLfloat -> ReaderT Font m b -> m b
withSharedFont font@Font{..} projViewM44 renderActions = do
    let GlyphUniforms{..} = fntUniforms
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)
    uniformI   uTexture 0

    uniformF   uTime =<< getNow
    uniformM44 uProjectionView projViewM44

    runReaderT renderActions font

renderTextPreCorrectedOfSameFont :: (MonadIO m, MonadReader Font m)
                                 => TextRenderer -> M44 GLfloat -> m ()
renderTextPreCorrectedOfSameFont textRenderer modelM44 = do
    needsBufferUpload <- liftIO . atomically $ do
        let needsBufferUploadVar = textRenderer ^. txrNeedsBufferUploadVar
        result <- readTVar needsBufferUploadVar
        when (result == True) (writeTVar needsBufferUploadVar False)
        return result
    when needsBufferUpload $ do
        uploadCharacters textRenderer

    Font{..} <- ask
    let rendererVAO       = textRenderer ^. txrVAO
        TextMetrics{..}   = textRenderer ^. txrTextMetrics
        GlyphUniforms{..} = fntUniforms

    uniformM44 uModel modelM44

    let numVertices  = 4
        numInstances = fromIntegral txmNumChars
    withVAO rendererVAO $
        glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()
