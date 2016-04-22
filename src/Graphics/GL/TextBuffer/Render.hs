{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
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
                                        , txmNumChars = 0 
                                        }
        , _txrVAO                = glyphVAO
        , _txrCorrectionM44      = identity
        , _txrIndexBuffer        = glyphIndexBuffer
        , _txrOffsetBuffer       = glyphOffsetBuffer
        , _txrFont               = font
        , _txrDragRoot           = Nothing
        , _txrFileEventListener  = Nothing
        , _txrScroll             = V2 0 0
        , _txrScreenSize         = Nothing
        }

-- | Recalculates the character indices and glyph offsets of a TextBuffer 
-- and writes them into the TextRenderer's ArrayBuffers
updateMetrics :: MonadIO m => TextRenderer -> m TextRenderer
updateMetrics textRenderer@TextRenderer{..} = do
    
    let newTextRenderer = textRenderer &~ do
            txrTextMetrics   .= calculateMetrics (textRenderer ^. txrTextBuffer) (textRenderer ^. txrFont)
            txrCorrectionM44 <~ (correctionMatrixForTextRenderer <$> use id) 
            id %= updateScroll
        newTextMetrics = newTextRenderer ^. txrTextMetrics
    liftIO . print $ newTextRenderer ^. txrScroll
    liftIO . print $ newTextRenderer ^. txrTextBuffer . to bufSelection
    bufferSubData (newTextRenderer ^. txrIndexBuffer)  (txmCharIndices newTextMetrics)
    bufferSubData (newTextRenderer ^. txrOffsetBuffer) (map snd (txmCharOffsets newTextMetrics))

    return newTextRenderer

updateScroll :: TextRenderer -> TextRenderer
updateScroll textRenderer = case (textRenderer ^. txrScreenSize, textRenderer ^. txrTextBuffer . to bufSelection) of
    (Just screenSize, Just (Cursor (fromIntegral -> cursorLine) (fromIntegral -> cursorCol), _)) -> 
        textRenderer &~ do
            let V2 fontWidth fontHeight = fontDimensions (textRenderer ^. txrFont)
            let V2 screenW screenHOrig = fromIntegral <$> screenSize
                screenH = screenHOrig * (fontWidth / fontHeight)
            V2 scrollX scrollY <- use txrScroll
            when (cursorCol > screenW + scrollX) $ 
                txrScroll . _x .= -(cursorCol - (screenW - 2))
            when (cursorCol < scrollX) $ 
                txrScroll . _x .= cursorCol
            when (cursorLine > screenH + scrollY) $
                txrScroll . _y .= cursorLine - (screenH - 2)
            when (cursorLine < scrollY) $ 
                txrScroll . _y .= cursorLine

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
                (-(screenW * fontWidth)  / 2)
                ( (screenH * fontHeight) / 2)
            scaling = 1 / fontWidth / screenW
            offset = baseOffset + (textRenderer ^. txrScroll * fontDims)

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


