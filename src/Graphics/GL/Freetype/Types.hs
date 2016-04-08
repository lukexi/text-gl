{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Freetype.Types where

import Graphics.GL.Pal
import Graphics.GL.Freetype.API

import System.IO.Unsafe

-- Used by GHC in its log messages
quoteChars :: String
quoteChars = "‘’"

-- Aka ASCII codes 32-126
asciiChars :: String
asciiChars = quoteChars ++ [' '..'~']

blockChar :: Char
blockChar = '█'

cursorChar :: Char
cursorChar = '▏'


data Glyph = Glyph
  { glyIndex    :: !GLint
  , glyGlyphPtr :: !GlyphPtr
  , glyMetrics  :: !GlyphMetrics
  } deriving Show

getGlyphKerning :: Glyph -> Char -> Float
getGlyphKerning glyph character = unsafePerformIO (getGlyphKerningIO (glyGlyphPtr glyph) character)

data GlyphUniforms = GlyphUniforms
  { uMVP             :: UniformLocation (M44 GLfloat)
  , uTexture         :: UniformLocation GLint
  , uColor           :: UniformLocation (V3 GLfloat)
  , uTime            :: UniformLocation GLfloat
  } deriving (Data, Show)

data Font = Font 
  { fntFontPtr       :: !FontPtr
  , fntAtlas         :: !TextureAtlas
  , fntTextureID     :: !TextureID
  , fntUniforms      :: !GlyphUniforms
  , fntShader        :: !Program
  , fntPointSize     :: !Float
  , fntGlyphForChar  :: !(Char -> Glyph)
  }
instance Show Font where
  show _ = "Font {}"
