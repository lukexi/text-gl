{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.Freetype.Types where

import Graphics.GL.Pal

import Data.Map (Map)
import Data.Sequence (Seq)

import Graphics.GL.Freetype.API
import Control.Lens

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



data TextBuffer = TextBuffer 
  { bufSelection    :: !(Int, Int)
  , bufColumn       :: !Int
  , bufText         :: !(Seq Char)
  , bufPath         :: !FilePath
  , bufUndo         :: !(Maybe TextBuffer)
  } deriving Show

data TextMetrics = TextMetrics
  { txmCharIndices :: ![GLint]
  , txmCharOffsets :: ![V2 GLfloat]
  , txmNumChars    :: !Int
  }

data TextRenderer = TextRenderer
  { _txrFont         :: !Font
  , _txrVAO          :: !VertexArrayObject
  , _txrIndexBuffer  :: !ArrayBuffer
  , _txrOffsetBuffer :: !ArrayBuffer
  , _txrTextBuffer   :: !TextBuffer
  , _txrTextMetrics  :: !TextMetrics
  }
makeLenses ''TextRenderer
