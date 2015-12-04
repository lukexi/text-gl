{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.GL.Freetype.Render where

import Graphics.GL.Freetype.API

import Graphics.GL.Pal

import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Foldable

import Graphics.GL.Freetype.Types



createFont :: String -> Float -> Program -> IO Font
createFont fontFile pointSize shader = createFontWithChars fontFile pointSize shader asciiChars

createFontWithChars :: String -> Float -> Program -> String -> IO Font
createFontWithChars fontFile pointSize shader characters = do
    let allCharacters = cursorChar:blockChar:characters
    -- Create an atlas to hold the characters
    atlas  <- newTextureAtlas 1024 1024 BitDepth1
    -- Create a font and associate it with the atlas
    font   <- newFontFromFile atlas pointSize fontFile
    -- Load the characters into the atlas
    missed <- loadFontGlyphs font allCharacters
    when (missed > 0) $
      putStrLn ("Tried to load too many characters! Missed: " ++ show missed)
    
    let textureID = TextureID (atlasTextureID atlas)

    
    (characterMetrics, glyphsByChar) <- foldM (\(allCharacterMetrics, glyphsByChar) (character, i) -> do

      glyphPtr                 <- getGlyph font character
      metrics@GlyphMetrics{..} <- getGlyphMetrics glyphPtr

      let x0  = gmOffsetX
          y0  = gmOffsetY
          x1  = gmOffsetX + gmWidth
          y1  = gmOffsetY - gmHeight

          charPositions = concatMap toList
                    [ V4 x0 y1 gmS0 gmT1
                    , V4 x0 y0 gmS0 gmT0  
                    , V4 x1 y1 gmS1 gmT1  
                    , V4 x1 y0 gmS1 gmT0 ] :: [GLfloat]

          charMetricsStructureFlattened = charPositions

          glyph                  = Glyph { glyIndex = i, glyGlyphPtr = glyphPtr, glyMetrics = metrics }
          newAllCharacterMetrics = allCharacterMetrics ++ charMetricsStructureFlattened
          newGlyphsByChar        = Map.insert character glyph glyphsByChar
      -- print character
      -- print (charPositions)
      return (newAllCharacterMetrics, newGlyphsByChar)
      ) mempty (zip allCharacters [0..])

    charMetricsBuffer <- bufferUniformData GL_STATIC_DRAW characterMetrics

    -- Set up our UBO globally
    let charMetricsBindingPoint = UniformBlockBindingPoint 0
    bindUniformBufferBase charMetricsBuffer charMetricsBindingPoint

    -- Bind the shader's uniform buffer declaration to the correct uniform buffer object
    bindShaderUniformBuffer shader "charactersBlock" charMetricsBindingPoint

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

    uniforms <- acquireUniforms shader

    let !blockGlyph = glyphsByChar ! blockChar
        glyphForChar aChar = case Map.lookup aChar glyphsByChar of
          Just glyph -> glyph
          Nothing    -> blockGlyph
    return Font
      { fntFontPtr            = font
      , fntAtlas              = atlas
      , fntTextureID          = textureID
      , fntUniforms           = uniforms
      , fntShader             = shader
      , fntPointSize          = pointSize
      , fntVAO                = glyphVAO
      , fntIndexBuffer        = glyphIndexBuffer
      , fntOffsetBuffer       = glyphOffsetBuffer
      , fntGlyphForChar       = glyphForChar
      }


renderText :: (Foldable f, MonadIO m) 
            => Font -> V3 GLfloat -> f Char -> M44 GLfloat -> m ()
renderText font@Font{..} color string mvp = do
    useProgram fntShader
    glBindTexture GL_TEXTURE_2D (unTextureID fntTextureID)

    let GlyphUniforms{..} = fntUniforms
        correctedMVP = mvp !*! correctionMatrixForFont font
                           
    uniformM44 uMVP     correctedMVP
    uniformI   uTexture 0
    uniformV3  uColor   color


    let numVertices  = 4
        -- Add 1 to ensure we still render the cursor
        numInstances = fromIntegral (length string + 1)
    withVAO fntVAO $ 
      glDrawArraysInstanced GL_TRIANGLE_STRIP 0 numVertices numInstances
    return ()

correctionMatrixForFont :: Fractional a => Font -> M44 a
correctionMatrixForFont Font{..} = correctedMVP
  where
    -- Ensures the characters are always the same 
    -- size no matter what point size was specified
    resolutionCompensationScale = realToFrac (1 / fntPointSize / charWidth)
    -- Also scale by the width of a wide character
    charWidth = gmAdvanceX (glyMetrics (fntGlyphForChar '_'))
    correctedMVP = translateMatrix (V3 (-0.5) (0.5) 0) 
               !*! scaleMatrix resolutionCompensationScale