{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.TextBuffer.Types where

import Graphics.GL.Pal

import Data.Map (Map)
import Data.Sequence (Seq)

import Graphics.GL.Freetype.API
import Control.Lens.Extra

import System.IO.Unsafe

import Graphics.GL.Freetype.Types

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
