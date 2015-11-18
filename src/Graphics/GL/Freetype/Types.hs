{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Freetype.Types where

import Graphics.GL.Pal

import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Foldable

import Graphics.GL.Freetype.API

data GlyphUniforms = GlyphUniforms
  { uMVP             :: UniformLocation (M44 GLfloat)
  , uTexture         :: UniformLocation GLint
  , uColor           :: UniformLocation (V3 GLfloat)
  } deriving Data


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
  }

data Glyph = Glyph
  { glyIndex    :: GLint
  , glyGlyphPtr :: GlyphPtr
  , glyMetrics  :: GlyphMetrics
  }

data TextBuffer = TextBuffer 
  { bufSelection :: !(Int, Int)
  , bufColumn    :: !Int
  , bufText      :: !(Seq Char)
  , bufPath      :: !FilePath
  } deriving Show
