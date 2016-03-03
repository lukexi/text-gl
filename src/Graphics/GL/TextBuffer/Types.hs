{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.TextBuffer.Types where

import Graphics.GL.Pal

import Data.Sequence (Seq)

import Control.Lens.Extra

import Graphics.GL.Freetype.Types

data TextBuffer = TextBuffer 
  { bufSelection    :: !(Maybe (Int, Int))
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
