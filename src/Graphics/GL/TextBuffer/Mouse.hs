{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}


module Graphics.GL.TextBuffer.Mouse where

import Graphics.VR.Pal
import SDL hiding (Cursor)

import Graphics.GL.Pal hiding (trace)
import Control.Lens.Extra

import Control.Monad.Reader
import Control.Monad.State

import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Render
import Graphics.GL.TextBuffer.Types


handleTextBufferMouseEvent :: forall s m. (MonadState s m, MonadIO m)
                           => Window -> Event -> Traversal' s TextRenderer -> M44 GLfloat -> M44 GLfloat -> Pose GLfloat -> m ()
handleTextBufferMouseEvent win e rendererLens projM44 modelM44 playerPose = do
    onMouse1Down e $ rendererLens >>~ \textRenderer -> do
        ray <- cursorPosToWorldRay win projM44 playerPose
        case rayToTextRendererCursor ray textRenderer modelM44 of
            Just cursor -> rendererLens %=~ beginDrag cursor
            Nothing -> return ()
    onMouseMove e $ \_ -> rendererLens >>~ \textRenderer -> do
        ray <- cursorPosToWorldRay win projM44 playerPose
        let _ = ray :: Ray GLfloat
        case rayToTextRendererCursor ray textRenderer modelM44 of
            Just cursor -> rendererLens %=~ continueDrag cursor
            Nothing -> return ()
    onMouse1Up e $ do
        rendererLens %=~ endDrag

-- | This is quick and dirty.
-- We should keep the bounding boxes of the characters during
-- calculateIndicesAndOffsets, which we should cache in the TextBuffer.
-- We currently use the point size which is quite wrong
rayToTextRendererCursor :: Ray Float -> TextRenderer -> M44 Float -> (Maybe Cursor)
rayToTextRendererCursor ray textRenderer model44 =
    let textModel44   = model44 !*! textRenderer ^. txrCorrectionM44
        font          = textRenderer ^. txrFont
        aabb          = (0, V3 1 (-1) 0) -- FIXME: this assumes a 1x1 (GL units) text pane; we should support any size
        mIntersection = rayOBBIntersection ray aabb textModel44
    in join . flip fmap mIntersection $ \intersectionDistance ->
        let worldPoint       = projectRay ray intersectionDistance
            V3 cursX cursY _ = worldPointToModelPoint textModel44 worldPoint
            -- The width here is a guess; should get this from GlyphMetrics
            -- during updateIndicesAndOffsets
            V2 charW charH   = fontDimensions font
            charOffsets      = txmCharOffsets (textRenderer ^. txrTextMetrics)
            hits = filter (\(_i, V4 x y _ _) ->
                           cursX > x
                        && cursX < (x + charW)
                        && cursY > y
                        && cursY < (y + charH))
                    charOffsets
        in case hits of
            ((i, _):_) -> Just i
            []         -> Nothing

setCursorTextRendererWithRay :: MonadIO m => Ray GLfloat -> TextRenderer -> M44 GLfloat -> m TextRenderer
setCursorTextRendererWithRay ray textRenderer model44 =
    case rayToTextRendererCursor ray textRenderer model44 of
        Just i  -> updateMetrics (textRenderer & txrTextBuffer %~ moveTo i)
        Nothing -> return textRenderer

beginDrag :: MonadIO m => Cursor -> TextRenderer -> m TextRenderer
beginDrag cursor textRenderer =
    updateMetrics (textRenderer & txrTextBuffer %~ moveTo cursor
                                & txrDragRoot ?~ cursor)

continueDrag :: MonadIO m => Cursor -> TextRenderer -> m TextRenderer
continueDrag cursor textRenderer = case textRenderer ^. txrDragRoot of
    Nothing -> return textRenderer
    Just dragRoot -> updateMetrics (textRenderer & txrTextBuffer %~ setSelection newSel)
        where newSel = if cursor > dragRoot then (dragRoot, cursor) else (cursor, dragRoot)

endDrag :: MonadIO m => TextRenderer -> m TextRenderer
endDrag textRenderer =
    updateMetrics (textRenderer & txrDragRoot .~ Nothing)
