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
import Control.Exception

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
        `catch` (\e -> do
            putStrLn $ "textRendererFromFile couldn't read " ++ filePath ++
                ": " ++ show (e::SomeException)
            return "")
    textRenderer <- createTextRenderer font (textBufferWithPath filePath text)

    case watchMode of
        NoWatchFile -> return textRenderer
        WatchFile -> do
            fileEventListener <- eventListenerForFile filePath ReadFileOnEvents
            return $ textRenderer & txrFileEventListener ?~ fileEventListener

renameTextRendererFile :: (MonadIO m)
                       => FilePath -> TextRenderer -> m TextRenderer
renameTextRendererFile newFileName textRenderer = do
    forM_ (textRenderer ^. txrFileEventListener) killFileEventListener

    fileEventListener <- eventListenerForFile newFileName ReadFileOnEvents
    return $ textRenderer &~ do
        txrFileEventListener ?= fileEventListener
        txrTextBuffer %= \textBuf -> textBuf { bufPath = Just newFileName }

-- | Must pass WatchFile to textRendererFromFile to use this
refreshTextRendererFromFile :: forall s m. (MonadState s m, MonadIO m)
                            => Traversal' s TextRenderer -> m ()
refreshTextRendererFromFile rendererLens = do
    rendererLens >>~ \textRenderer ->
        forM_ (textRenderer ^. txrFileEventListener) $ \fileEventListener ->
            tryReadTChanIO (felEventTChan fileEventListener) >>= \case
                Just (Right newText) -> setTextRendererText rendererLens newText
                Just (Left  _) -> liftIO (putStrLn "Couldn't refresh text renderer, FileEventListener wasn't configured to read the file")
                Nothing -> return ()

editTextRendererBuffer :: forall s m. (MonadState s m, MonadIO m)
                       => Traversal' s TextRenderer -> (TextBuffer -> TextBuffer) -> m ()
editTextRendererBuffer rendererLens action = do
    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer
    textBufferLens %= action
    rendererLens %=~ updateMetrics


setTextRendererText :: forall s m. (MonadState s m, MonadIO m)
                    => Traversal' s TextRenderer -> String -> m ()
setTextRendererText rendererLens text =
    editTextRendererBuffer rendererLens (setTextFromString text)

saveTextBuffer :: MonadIO m => TextBuffer -> m ()
saveTextBuffer buffer = liftIO $ case bufPath buffer of
    Nothing -> putStrLn "Tried to save text buffer with no path"
    Just bufferPath -> do
        --putStrLn $ "Saving " ++ bufferPath ++ "..."
        writeFile bufferPath (stringFromTextBuffer buffer)
            `catch` (\e ->
                putStrLn ("Error in saveTextBuffer: " ++ show (e :: IOException)))

handleTextBufferEvent :: forall s m. (MonadState s m, MonadIO m)
                      => Window -> Event -> Traversal' s TextRenderer -> m ()
handleTextBufferEvent win e rendererLens = do

    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer

        updateBuffer editAction causesSave = do
            editTextRendererBuffer rendererLens editAction

            when causesSave $
                textBufferLens >>~ void . liftIO . forkIO . saveTextBuffer
    -- Copy
    onKeyWithMods e [controlModKey] Key'C $ do
        textBufferLens >>~ setClipboardString win . selectionFromTextBuffer
    -- Cut
    onKeyWithMods e [controlModKey] Key'X $ do
        textBufferLens >>~ setClipboardString win . selectionFromTextBuffer
        updateBuffer backspace True
    -- Paste
    onKeyWithMods e [controlModKey] Key'V $ do
        string <- fromMaybe "" <$> getClipboardString win
        updateBuffer (insertString string) True

    forM_ keyCommands $ \KeyCommand{..} ->
        onKeyWithMods e kcmModKeys kcmKey (updateBuffer kcmAction kcmCausesSave)

    -- Regular character insertion
    onChar e $ \case
        (isBackspaceChar -> True) -> updateBuffer backspace True
        char                      -> updateBuffer (insertChar char) True

eventWillSaveTextBuffer :: Event -> Bool
eventWillSaveTextBuffer e = runIdentity $ do
    commands <- forM keyCommands $ \KeyCommand{..} ->
        ifKeyWithMods False e kcmModKeys kcmKey
            (return kcmCausesSave)
    charCommand <- ifChar False e (\_ -> return True)
    return $ or (charCommand:commands)


controlModKey, optionModKey :: ModKey
--(controlModKey, optionModKey) = (ModKeySuper, ModKeyAlt) -- Mac
(controlModKey, optionModKey) = (ModKeyControl, ModKeyAlt) -- Windows

type CausesSave = Bool
data KeyCommand = KeyCommand
    { kcmCausesSave :: CausesSave
    , kcmModKeys    :: [ModKey]
    , kcmKey        :: Key
    , kcmAction     :: (TextBuffer -> TextBuffer)
    }

keyCommands :: [KeyCommand]
keyCommands =
    --           Save? ModKeys                      Key              Action
    [ KeyCommand False [controlModKey]              Key'C            id -- handled above
    , KeyCommand True  [controlModKey]              Key'X            id -- handled above
    , KeyCommand True  [controlModKey]              Key'V            id -- handled above
    , KeyCommand True  [controlModKey]              Key'Z            undo
    , KeyCommand True  []                           Key'Enter        carriageReturn
    , KeyCommand True  [controlModKey]              Key'Enter        carriageReturnToNextLine
    , KeyCommand True  []                           Key'Backspace    backspace
    , KeyCommand False []                           Key'Left         moveLeft
    , KeyCommand False []                           Key'Right        moveRight
    , KeyCommand False []                           Key'Down         moveDown
    , KeyCommand False []                           Key'Up           moveUp
    , KeyCommand False [optionModKey]               Key'Left         moveWordLeft
    , KeyCommand False [optionModKey]               Key'Right        moveWordRight
    , KeyCommand False [ModKeyShift]                Key'Left         selectLeft
    , KeyCommand False [ModKeyShift]                Key'Right        selectRight
    , KeyCommand False [ModKeyShift]                Key'Up           selectUp
    , KeyCommand False [ModKeyShift]                Key'Down         selectDown
    , KeyCommand False [optionModKey, ModKeyShift]  Key'Right        selectWordRight
    , KeyCommand False [optionModKey, ModKeyShift]  Key'Left         selectWordLeft
    , KeyCommand False [controlModKey]              Key'B            moveLeft
    , KeyCommand False [controlModKey]              Key'F            moveRight
    , KeyCommand True  [controlModKey, ModKeyShift] Key'Up           moveLinesUp
    , KeyCommand True  [controlModKey, ModKeyShift] Key'Down         moveLinesDown
    , KeyCommand True  [controlModKey]              Key'RightBracket indentLines
    , KeyCommand True  [controlModKey]              Key'LeftBracket  unindentLines
    , KeyCommand True  [controlModKey]              Key'Slash        toggleLinesComment
    , KeyCommand True  [controlModKey, ModKeyShift] Key'D            duplicateLine
    ]
