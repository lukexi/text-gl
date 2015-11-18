{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Freetype.Types where

import Graphics.GL.Pal

import Data.Map (Map)
import Data.Sequence (Seq)

import Graphics.GL.Freetype.API

-- Aka ASCII codes 32-126
asciiChars :: String
asciiChars = cursorChar:blockChar:[' '..'~']

blockChar :: Char
blockChar = '█'

cursorChar :: Char
cursorChar = '▏'

data GlyphUniforms = GlyphUniforms
  { uMVP             :: UniformLocation (M44 GLfloat)
  , uTexture         :: UniformLocation GLint
  , uColor           :: UniformLocation (V3 GLfloat)
  } deriving (Data, Show)


data Font = Font 
  { fntFontPtr       :: FontPtr
  , fntAtlas         :: TextureAtlas
  , fntTextureID     :: TextureID
  , fntUniforms      :: GlyphUniforms
  , fntShader        :: Program
  , fntPointSize     :: Float
  , fntGlyphsByChar  :: Map Char Glyph
  , fntVAO           :: VertexArrayObject
  , fntIndexBuffer   :: ArrayBuffer
  , fntOffsetBuffer  :: ArrayBuffer
  } deriving Show

data Glyph = Glyph
  { glyIndex    :: GLint
  , glyGlyphPtr :: GlyphPtr
  , glyMetrics  :: GlyphMetrics
  } deriving Show

data TextBuffer = TextBuffer 
  { bufSelection :: !(Int, Int)
  , bufColumn    :: !Int
  , bufText      :: !(Seq Char)
  , bufPath      :: !FilePath
  , bufFont      :: !Font
  } deriving Show
