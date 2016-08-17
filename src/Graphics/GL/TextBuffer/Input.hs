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

import Graphics.VR.Pal
import SDL

import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import Control.Exception

import Graphics.GL.Freetype
import Graphics.GL.TextBuffer.Types
import Graphics.GL.TextBuffer.TextBuffer
import Graphics.GL.TextBuffer.Render
--import Data.Char
import Control.Concurrent
import Halive.FileListener
import qualified Data.Text as Text

data FileWatchMode = WatchFile | NoWatchFile deriving (Eq, Show)

-- | Recognize certain control characters and react to them appropriately
--isBackspaceChar :: Char -> Bool
--isBackspaceChar = (== 8) . ord

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
                      => Event -> Traversal' s TextRenderer -> m ()
handleTextBufferEvent e rendererLens = do

    let textBufferLens :: Traversal' s TextBuffer
        textBufferLens = rendererLens . txrTextBuffer

        updateBuffer editAction causesSave = do
            editTextRendererBuffer rendererLens editAction

            when causesSave $
                textBufferLens >>~ void . liftIO . forkIO . saveTextBuffer
    -- Copy
    onKeyWithMods e [controlModKey] KeycodeC $ do
        textBufferLens >>~ setClipboardText . Text.pack . selectionFromTextBuffer
    -- Cut
    onKeyWithMods e [controlModKey] KeycodeX $ do
        textBufferLens >>~ setClipboardText . Text.pack . selectionFromTextBuffer
        updateBuffer backspace True
    -- Paste
    onKeyWithMods e [controlModKey] KeycodeV $ do
        string <- Text.unpack <$> getClipboardText
        updateBuffer (insertString string) True

    forM_ keyCommands $ \KeyCommand{..} ->
        onKeyWithMods e kcmModKeys kcmKey (updateBuffer kcmAction kcmCausesSave)

    -- Regular character insertion
    onText e $ \case
        --(isBackspaceChar -> True) -> updateBuffer backspace True
        text                      -> updateBuffer (insertString (Text.unpack text)) True

eventWillSaveTextBuffer :: Event -> Bool
eventWillSaveTextBuffer e = runIdentity $ do
    commands <- forM keyCommands $ \KeyCommand{..} ->
        ifKeyWithMods False e kcmModKeys kcmKey
            (return kcmCausesSave)
    charCommand <- ifText False e (\_ -> return True)
    return $ or (charCommand:commands)


controlModKey, optionModKey :: ModKey
--(controlModKey, optionModKey) = (ModKeySuper, ModKeyAlt) -- Mac
(controlModKey, optionModKey) = (ModKeyControl, ModKeyAlt) -- Windows

type CausesSave = Bool
data KeyCommand = KeyCommand
    { kcmCausesSave :: CausesSave
    , kcmModKeys    :: [ModKey]
    , kcmKey        :: Keycode
    , kcmAction     :: (TextBuffer -> TextBuffer)
    }

keyCommands :: [KeyCommand]
keyCommands =
    --           Save? ModKeys                      Key              Action
    [ KeyCommand False [controlModKey]              KeycodeC            id -- handled above
    , KeyCommand True  [controlModKey]              KeycodeX            id -- handled above
    , KeyCommand True  [controlModKey]              KeycodeV            id -- handled above
    , KeyCommand True  [controlModKey]              KeycodeZ            undo
    , KeyCommand True  []                           KeycodeReturn       carriageReturn
    , KeyCommand True  [controlModKey]              KeycodeReturn       carriageReturnToNextLine
    , KeyCommand True  []                           KeycodeBackspace    backspace
    , KeyCommand False []                           KeycodeLeft         moveLeft
    , KeyCommand False []                           KeycodeRight        moveRight
    , KeyCommand False []                           KeycodeDown         moveDown
    , KeyCommand False []                           KeycodeUp           moveUp
    , KeyCommand False [optionModKey]               KeycodeLeft         moveWordLeft
    , KeyCommand False [optionModKey]               KeycodeRight        moveWordRight
    , KeyCommand False [ModKeyShift]                KeycodeLeft         selectLeft
    , KeyCommand False [ModKeyShift]                KeycodeRight        selectRight
    , KeyCommand False [ModKeyShift]                KeycodeUp           selectUp
    , KeyCommand False [ModKeyShift]                KeycodeDown         selectDown
    , KeyCommand False [optionModKey, ModKeyShift]  KeycodeRight        selectWordRight
    , KeyCommand False [optionModKey, ModKeyShift]  KeycodeLeft         selectWordLeft
    , KeyCommand False [controlModKey]              KeycodeB            moveLeft
    , KeyCommand False [controlModKey]              KeycodeF            moveRight
    , KeyCommand True  [controlModKey, ModKeyShift] KeycodeUp           moveLinesUp
    , KeyCommand True  [controlModKey, ModKeyShift] KeycodeDown         moveLinesDown
    , KeyCommand True  [controlModKey]              KeycodeRightBracket indentLines
    , KeyCommand True  [controlModKey]              KeycodeLeftBracket  unindentLines
    , KeyCommand True  [controlModKey]              KeycodeSlash        toggleLinesComment
    , KeyCommand True  [controlModKey, ModKeyShift] KeycodeD            duplicateLine
    ]
