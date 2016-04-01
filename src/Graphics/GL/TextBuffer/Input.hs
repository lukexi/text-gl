{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.GL.TextBuffer.Input where

import Graphics.UI.GLFW.Pal

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State

import Graphics.GL.Freetype
import Graphics.GL.TextBuffer.Types
import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Render
import Data.Char
import Control.Concurrent
import Halive.FileListener
import Data.IORef

data FileWatchMode = WatchFile | NoWatchFile deriving (Eq, Show)

-- | Recognize certain control characters and react to them appropriately
isBackspaceChar :: Char -> Bool
isBackspaceChar = (== 8) . ord

-- | Must call refreshTextRendererFromFile
textRendererFromFile :: MonadIO m => Font -> FilePath -> FileWatchMode -> m TextRenderer
textRendererFromFile font filePath watchMode = liftIO $ do
    text <- readFile filePath
    textRenderer <- createTextRenderer font (textBufferWithPath filePath text)

    case watchMode of
        NoWatchFile -> return textRenderer
        WatchFile -> do
            fileEventListener <- eventListenerForFile filePath ReadFileOnEvents
            return $ textRenderer & txrFileEventListener ?~ fileEventListener

-- | Must pass WatchFile to textRendererFromFile to use this
refreshTextRendererFromFile :: forall s m. (MonadState s m, MonadIO m) 
                            => Traversal' s TextRenderer -> m ()
refreshTextRendererFromFile rendererLens = do
    
    mTextRenderer <- preuse rendererLens
    forM_ mTextRenderer $ \textRenderer -> 
        forM_ (textRenderer ^. txrFileEventListener) $ \fileEventListener -> 
            forM_ (bufPath (textRenderer ^. txrTextBuffer)) $ \filePath -> do
                tryReadTChanIO (felEventTChan fileEventListener) >>= \case
                    Just (Right newText) -> setTextRendererText rendererLens newText
                    Just (Left  _) -> liftIO (putStrLn "Couldn't refresh text renderer, FileEventListener wasn't configured to read the file")
                    Nothing -> return () 
                    

setTextRendererText :: forall s m. (MonadState s m, MonadIO m) 
                    => Traversal' s TextRenderer -> String -> m ()
setTextRendererText rendererLens text = do
    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer
    textBufferLens %= setTextFromString text
    updateTextBufferMetricsWithLens rendererLens

updateTextBufferMetricsWithLens :: forall s m. (MonadState s m, MonadIO m) 
                                 => Traversal' s TextRenderer -> m ()
updateTextBufferMetricsWithLens rendererLens = do
    mTextRenderer <- preuse rendererLens
    forM_ mTextRenderer $ \textRenderer -> do
        newRenderer <- updateMetrics textRenderer
        rendererLens .= newRenderer

saveTextBuffer :: MonadIO m => TextBuffer -> m ()
saveTextBuffer buffer = liftIO $ case bufPath buffer of
    Nothing -> putStrLn "Tried to save text buffer with no path"
    Just bufferPath -> do
        putStrLn $ "Saving " ++ bufferPath ++ "..."
        writeFile bufferPath (stringFromTextBuffer buffer)

-- Returns True if the given event caused a save action in the file
handleTextBufferEvent :: forall s m. (MonadState s m, MonadIO m) 
                      => Window -> Event -> Traversal' s TextRenderer -> m Bool
handleTextBufferEvent win e rendererLens = do
    -- FIXME there's certainly a cleaner way to do this;
    -- get rid of the onKey___ things and just iterate a list of bindings
    didSave <- liftIO (newIORef False)
    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer
        --(commandKey, commandModKey, optionModKey) = (Key'LeftSuper, ModKeySuper, ModKeyAlt) -- Mac
        (commandKey, _commandModKey, optionModKey) = (Key'LeftControl, ModKeyControl, ModKeyAlt) -- Windows
        -- Continuously save the file
        updateBuffer editAction causesSave = do
            textBufferLens %= editAction
            updateTextBufferMetricsWithLens rendererLens
            when causesSave $ do
                maybeRenderer <- preuse rendererLens
                forM_ maybeRenderer $ \renderer -> do
                    -- Save on a background thread
                    liftIO . forkIO $ saveTextBuffer (renderer ^. txrTextBuffer)
                liftIO (writeIORef didSave True)

    commandIsDown <- (== KeyState'Pressed) <$> getKey win commandKey
    -- shiftIsDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
    if  | commandIsDown -> do
            -- Manual save (not really necessary!)
            onKeyDown e Key'S      $ updateBuffer (id) True
            -- Copy
            onKeyDown e Key'C      $ do
                mTextBuffer <- preuse textBufferLens
                forM_ mTextBuffer $ \buffer -> setClipboardString win (selectionFromTextBuffer buffer)
            -- Cut
            onKeyDown e Key'X      $ do
                mTextBuffer <- preuse textBufferLens
                forM_ mTextBuffer $ \buffer -> setClipboardString win (selectionFromTextBuffer buffer)
                updateBuffer backspace True
            -- Paste
            onKeyDown e Key'V      $ do
                mString <- getClipboardString win
                forM_ mString (\string -> updateBuffer (insertString string) True)
            onKeyDown e Key'Z      $ 
                updateBuffer undo True
        | otherwise -> do

            onChar e $ \case 
                (isBackspaceChar -> True)                        -> updateBuffer backspace True
                char                                             -> updateBuffer (insertChar char) True
            onKey         e Key'Enter                             $ updateBuffer carriageReturn True
            onKey         e Key'Backspace                         $ updateBuffer backspace True
            onKey         e Key'Left                              $ updateBuffer moveLeft False
            onKey         e Key'Right                             $ updateBuffer moveRight False
            onKey         e Key'Down                              $ updateBuffer moveDown False
            onKey         e Key'Up                                $ updateBuffer moveUp False

            onKeyWithMods e [optionModKey]   Key'Left             $ updateBuffer moveWordLeft False
            onKeyWithMods e [optionModKey]   Key'Right            $ updateBuffer moveWordRight False

            onKeyWithMods e [optionModKey, ModKeyShift] Key'Right $ updateBuffer selectWordRight False
            onKeyWithMods e [optionModKey, ModKeyShift] Key'Left  $ updateBuffer selectWordLeft False

            onKeyWithMods e [ModKeyShift] Key'Left                $ updateBuffer selectLeft False
            onKeyWithMods e [ModKeyShift] Key'Right               $ updateBuffer selectRight False
            onKeyWithMods e [ModKeyShift] Key'Up                  $ updateBuffer selectUp False
            onKeyWithMods e [ModKeyShift] Key'Down                $ updateBuffer selectDown False
    liftIO (readIORef didSave)


