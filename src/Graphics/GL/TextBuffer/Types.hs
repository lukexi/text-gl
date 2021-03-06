{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.GL.TextBuffer.Types where

import Graphics.GL.Pal

import Data.Sequence (Seq)

import Control.Lens.Extra

import Graphics.GL.Freetype.Types

import Halive.FileListener
import Control.Concurrent.STM

type TextSeq = Seq (Seq Char)
type ColNum = Int
type LineNum = Int
data Cursor = Cursor LineNum ColNum deriving (Eq, Show, Ord)
type Selection = (Cursor, Cursor)

data TextBuffer = TextBuffer
    { bufSelection    :: !(Maybe Selection)
    , bufColumn       :: !Int
    , bufText         :: !TextSeq
    , bufPath         :: !(Maybe FilePath)
    , bufUndo         :: !(Maybe TextBuffer)
    , bufDims         :: !(Int, Int)
    } deriving Show

data TextMetrics = TextMetrics
    { txmCharIndices :: ![GLint]
    , txmCharOffsets :: ![(Cursor, V4 GLfloat)]
    , txmNumChars    :: !Int
    } deriving Show

data TextRendererResources = TextRendererResources
    { _trrVAO                  :: !VertexArrayObject
    , _trrIndexBuffer          :: !(ArrayBuffer GLint)
    , _trrOffsetBuffer         :: !(ArrayBuffer (V4 GLfloat))
    }
makeLenses ''TextRendererResources

data TextRenderer = TextRenderer
    { _txrFont                 :: !Font
    , _txrRenderResourcesVar   :: !(TMVar TextRendererResources)
    , _txrRenderChan           :: !(TChan TextRenderer)
    , _txrCorrectionM44        :: !(M44 GLfloat)
    , _txrTextBuffer           :: !TextBuffer
    , _txrTextMetrics          :: !TextMetrics
    , _txrDragRoot             :: !(Maybe Cursor)
    , _txrFileEventListener    :: !(Maybe FileEventListener)
    , _txrScroll               :: !(V2 GLfloat)
    , _txrScreenSize           :: !(Maybe (V2 Int))
    }
makeLenses ''TextRenderer
