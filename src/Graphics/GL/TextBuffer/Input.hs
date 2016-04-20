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
import Data.Maybe

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
                      => Window -> Event -> Traversal' s TextRenderer -> m ()
handleTextBufferEvent win e rendererLens = do

    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer
        
        updateBuffer editAction causesSave = do
            textBufferLens %= editAction
            updateTextBufferMetricsWithLens rendererLens
            when causesSave $ do
                mTextBuffer <- preuse textBufferLens
                forM_ mTextBuffer $ \textBuffer -> do
                    -- Save on a background thread
                    liftIO . forkIO $ saveTextBuffer textBuffer
    -- Copy
    onKeyWithMods e [commandModKey]                Key'C      $ do
        mTextBuffer <- preuse textBufferLens
        forM_ mTextBuffer $ \buffer -> setClipboardString win (selectionFromTextBuffer buffer)
    -- Cut
    onKeyWithMods e [commandModKey]                Key'X      $ do
        mTextBuffer <- preuse textBufferLens
        forM_ mTextBuffer $ \buffer -> setClipboardString win (selectionFromTextBuffer buffer)
        updateBuffer backspace True

    -- Paste
    onKeyWithMods e [commandModKey]                Key'V      $ do
        string <- fromMaybe "" <$> getClipboardString win
        updateBuffer (insertString string) True

    forM_ keyCommands $ \KeyCommand{..} -> 
        onKeyWithMods e kcmModKeys kcmKey (updateBuffer kcmAction kcmCausesSave) 

    -- Regular character insertion
    onChar e $ \case 
        (isBackspaceChar -> True)                            -> updateBuffer backspace True
        char                                                 -> updateBuffer (insertChar char) True

willSaveTextBuffer :: Monad m => Event -> m Bool
willSaveTextBuffer e = do
    commands <- forM keyCommands $ \KeyCommand{..} -> 
        ifKey False e kcmKey (return kcmCausesSave)
    charCommand <- ifChar False e (\_ -> return True)
    return $ or (charCommand:commands)


commandModKey, optionModKey :: ModKey
--(commandModKey, optionModKey) = (ModKeySuper, ModKeyAlt) -- Mac
(commandModKey, optionModKey) = (ModKeyControl, ModKeyAlt) -- Windows

type CausesSave = Bool
data KeyCommand = KeyCommand 
    { kcmCausesSave :: CausesSave
    , kcmModKeys    :: [ModKey]
    , kcmKey        :: Key 
    , kcmAction     :: (TextBuffer -> TextBuffer)
    } 

keyCommands :: [KeyCommand]
keyCommands = 
    [ KeyCommand False [commandModKey]             Key'C         id -- handled above
    , KeyCommand True  [commandModKey]             Key'X         id -- handled above
    , KeyCommand True  [commandModKey]             Key'V         id -- handled above
    , KeyCommand True  [commandModKey]             Key'Z         undo
    , KeyCommand True  []                          Key'Enter     carriageReturn 
    , KeyCommand True  []                          Key'Backspace backspace 
    , KeyCommand False []                          Key'Left      moveLeft 
    , KeyCommand False []                          Key'Right     moveRight 
    , KeyCommand False []                          Key'Down      moveDown 
    , KeyCommand False []                          Key'Up        moveUp 
    , KeyCommand False [optionModKey]              Key'Left      moveWordLeft 
    , KeyCommand False [optionModKey]              Key'Right     moveWordRight 
    , KeyCommand False [optionModKey, ModKeyShift] Key'Right     selectWordRight 
    , KeyCommand False [optionModKey, ModKeyShift] Key'Left      selectWordLeft 
    , KeyCommand False [ModKeyShift]               Key'Left      selectLeft 
    , KeyCommand False [ModKeyShift]               Key'Right     selectRight 
    , KeyCommand False [ModKeyShift]               Key'Up        selectUp 
    , KeyCommand False [ModKeyShift]               Key'Down      selectDown 
    ]