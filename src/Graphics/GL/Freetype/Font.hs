{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.GL.Freetype.Font where

import Graphics.GL.Freetype.API

import Graphics.GL.Pal

import Control.Monad
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Foldable

import Graphics.GL.Freetype.Types
import Control.Monad.Trans


createFont :: MonadIO m => String -> Float -> Program -> m Font
createFont fontFile pointSize shader = createFontWithChars fontFile pointSize shader asciiChars

createFontWithChars :: MonadIO m => String -> Float -> Program -> String -> m Font
createFontWithChars fontFile pointSize shader characters = liftIO $ do
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

    -- We store our GlyphMetrics in a global uniform buffer.
    -- TODO this restricts us to one font at the moment... figure out how to
    -- get around that cleanly.
    -- Could use an array of Character arrays in the UBO and assign each font
    -- an index... but it feels like there must be a cleaner method that just 
    -- lets us pass a charMetricsBuffer ID directly to the shader.
    charMetricsBuffer <- bufferUniformData GL_STATIC_DRAW characterMetrics

    -- Set up our UBO globally
    let charMetricsBindingPoint = UniformBlockBindingPoint 0
    bindUniformBufferBase charMetricsBuffer charMetricsBindingPoint

    -- Bind the shader's uniform buffer declaration to the correct uniform buffer object
    bindShaderUniformBuffer shader "charactersBlock" charMetricsBindingPoint
    
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
      , fntGlyphForChar       = glyphForChar
      }




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
