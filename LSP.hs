
-- | An simple abstraction layer above the rather complex 
-- and low-level @haskell-lsp@ library.
--
-- (loosely based on the example server code in @haskell-lsp@)

{-# LANGUAGE BangPatterns, ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, GADTs, TypeInType #-}
{-# LANGUAGE TypeOperators, ExplicitNamespaces #-}
module LSP where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E 
import Control.Lens

import GHC.Generics (Generic)
import Control.DeepSeq

import Data.Default
import qualified Data.Text as T
import qualified Data.Rope.UTF16 as Rope

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Exit
import System.FilePath
import System.Directory
import System.IO.Unsafe as Unsafe

import qualified Data.Aeson               as J
import           Language.LSP.Server      as S
import           Language.LSP.Diagnostics
import qualified Language.LSP.Types       as J
import qualified Language.LSP.Types.Lens  as J
import           Language.LSP.VFS
import           System.Log.Logger        as L

import           Language.LSP.Types ( type (|?)(..) )

import Common

--------------------------------------------------------------------------------
-- * global config

lspServerName :: T.Text
lspServerName = T.pack "toy-ide"

logging :: Bool
logging = True

{-# NOINLINE logDir #-}
logDir :: FilePath
logDir = Unsafe.unsafePerformIO getTemporaryDirectory 

logFile        = logDir </> "toy-ide.log"
sessionLogFile = logDir </> "toy-ide-session.log"

--------------------------------------------------------------------------------
-- * our simplified LSP abstraction layer

data IDECompletion = IDECompletion
  { ideComplLabel   :: String 
  , ideComplReplace :: Maybe String
  , ideComplKind    :: Maybe J.CompletionItemKind
  }

data IDE result = IDE
  { ideCheckDocument :: T.Text -> result
  , ideDiagnostics   :: result -> [Diag]
  , ideOnHover       :: result -> SrcPos -> Maybe (Location,[String])
  , ideHighlight     :: result -> SrcPos -> [Location]
  , ideDefinLoc      :: result -> SrcPos -> Maybe Location
  , ideCompletion    :: result -> SrcPos -> [IDECompletion]
  , ideRename        :: result -> SrcPos -> String -> [(Location,String)]
  }

data Diag = Diag
  { diagLocation :: !Location
  , diagSeverity :: !J.DiagnosticSeverity
  , diagMessage  :: !String
  }
  deriving (Generic,NFData)
   
data IDEState result
  = IdeChecking !ThreadId    -- ^ we are in the process of checking the source
  | IdeChecked  !result      -- ^ checking is done, but no feedback sent yet
  | IdeDone     !result      -- ^ feedback was also sent

ideResult :: IDEState result -> Maybe result
ideResult state = case state of
  IdeChecking {} -> Nothing
  IdeChecked res -> Just res
  IdeDone    res -> Just res
  
type IDETable result = Map J.NormalizedUri (IDEState result)  

--------------------------------------------------------------------------------
-- * misc helpers

adjustMVar :: MVar a -> (a -> a) -> IO ()
adjustMVar mv f = do
  x <- takeMVar mv
  putMVar mv $! f x

mapReplaceIfExists :: Ord k => k -> v -> Map k v -> Map k v 
mapReplaceIfExists !k !v = Map.alter f k where
  f Nothing  = Nothing
  f (Just _) = Just v

--------------------------------------------------------------------------------
-- * completion items

-- why isn't this part of @haskell-lsp@?
defCompletionItem :: J.CompletionItem
defCompletionItem = J.CompletionItem
  { J._label = T.empty                 -- The label of this completion item. By default also the text that is inserted when selecting this completion.
  , J._kind  = Nothing                 -- Kind of the item (method, type, color, etc)
  , J._tags  = Nothing                 -- Tags for this completion item.
  , J._detail = Nothing                -- A human-readable string with additional information about this item, like type or symbol information.
  , J._documentation = Nothing         -- A human-readable string that represents a doc-comment.
  , J._deprecated = Nothing            -- Indicates if this item is deprecated.
  , J._preselect = Nothing             -- Select this item when showing. *Note* that only one completion item can be selected and that the tool / client decides which item that is. The rule is that the *first* item of those that match best is selected.
  , J._sortText = Nothing              -- A string that should be used when filtering a set of completion items. When falsy the label is used.
  , J._filterText = Nothing            -- A string that should be used when filtering a set of completion items. When falsy the label is used.
  , J._insertText = Nothing            -- A string that should be inserted a document when selecting this completion. When falsy the label is used.
  , J._insertTextFormat = Nothing      -- The format of the insert text. The format applies to both the insertText property and the newText property of a provided textEdit.
  , J._textEdit = Nothing              -- An edit which is applied to a document when selecting this completion. When an edit is provided the value of insertText is ignored.
  , J._additionalTextEdits = Nothing   -- An optional array of additional text edits that are applied when selecting this completion. Edits must not overlap with the main edit nor with themselves.
  , J._commitCharacters = Nothing      -- An optional set of characters that when pressed while this completion is active will accept it first and then type that character. *Note* that all commit characters should have `length=1` and that superfluous characters will be ignored.
  , J._command = Nothing               -- An optional command that is executed *after* inserting this completion. *Note* that additional modifications to the current document should be described with the additionalTextEdits-property.
  , J._xdata = Nothing 
  }
  
mkCompletionItem :: IDECompletion -> J.CompletionItem
mkCompletionItem (IDECompletion label mbreplace mbkind) = case mbreplace of
  Nothing -> defCompletionItem
    { J._label = T.pack label   
    , J._kind  = mbkind
    }
  Just replace -> defCompletionItem
    { J._label      = T.pack label   
    , J._kind       = mbkind
    , J._insertText = Just (T.pack replace)
    }

--------------------------------------------------------------------------------
-- * re-check documents

-- seriously...
class    SomeVersion v where versionToMaybeInt :: v -> Maybe Int
instance SomeVersion (Maybe Int) where versionToMaybeInt = id
instance SomeVersion Int         where versionToMaybeInt = Just
  
checkDocument 
  :: (NFData result, SomeVersion ver, J.HasUri textdoc J.Uri, J.HasVersion textdoc ver)
  => IDE result -> MVar (IDETable result) 
  -> textdoc -> Maybe FilePath -> T.Text -> LspM Config ()
checkDocument ide global tdoc mbFilePath text = do
  let uri = tdoc ^. J.uri . to J.toNormalizedUri
      ver = tdoc ^. J.version
  liftIO $ debugM "checkDocument" $ "checking document..."
  let !result = ideCheckDocument ide text
  -- liftIO $ print result
  rnf result `seq` (liftIO $ adjustMVar global (mapReplaceIfExists uri (IdeChecked result)))
  liftIO $ debugM "checkDocument" $ "computing diagnostics..."
  let !diags = ideDiagnostics ide result
  sendDiags uri (versionToMaybeInt ver) diags    -- version ???
  rnf diags `seq` (liftIO $ adjustMVar global (mapReplaceIfExists uri (IdeDone result)))
  liftIO $ debugM "checkDocument" $ "done."
  return ()
   
updateDocument 
  :: (NFData result, SomeVersion ver, J.HasUri textdoc J.Uri, J.HasVersion textdoc ver)
  => IDE result -> MVar (IDETable result) 
  -> textdoc -> Maybe FilePath -> T.Text -> LspM Config ()
updateDocument ide global tdoc mbFileName text = do
  let uri = tdoc ^. J.uri . to J.toNormalizedUri
  lspEnv <- getLspEnv  -- :: LanguageContextEnv config
  liftIO $ debugM "updateDocument" $ "updating document..."
  liftIO $ do
    table <- takeMVar global
    case Map.lookup uri table of
      Just (IdeChecking old_threadid) -> killThread old_threadid
      _ -> return () 
    threadid <- forkIO $ runLspT lspEnv $ checkDocument ide global tdoc mbFileName text
    putMVar global $! (Map.insert uri (IdeChecking threadid) table)

closeDocument 
  :: NFData result => IDE result -> MVar (IDETable result) 
  -> J.NormalizedUri -> LspM Config ()
closeDocument ide global uri = do
  liftIO $ adjustMVar global (Map.delete uri)
  
--------------------------------------------------------------------------------
-- * interfacing with @haskell-lsp@

lspMain :: forall result. NFData result => IDE result -> IO ()
lspMain ide = do
  global <- newMVar Map.empty :: IO (MVar (IDETable result))
  run ide global >>= \r -> case r of
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

run :: forall result. NFData result => IDE result -> MVar (IDETable result) -> IO Int
run ide global = flip E.catches handlers $ do
  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  let serverDefinition = ServerDefinition
        { onConfigurationChange = \v -> case J.fromJSON v of
            J.Error   e -> pure $ Left (T.pack e)
            J.Success cfg -> do
              -- sendNotification J.SWindowShowMessage $
              --  J.ShowMessageParams J.MtInfo $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))
              pure $ Right cfg
        , doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env)
        , staticHandlers = lspHandlers ide global rin
        , interpretHandler = \env -> S.Iso (runLspT env) liftIO
        , options = lspOptions
        }

  flip E.finally finalProc $ do
    setupLogger Nothing ["reactor"] DEBUG
    runServer serverDefinition

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

--------------------------------------------------------------------------------

-- | Server configuration
data Config 
  = Config 
  deriving (Generic, J.ToJSON, J.FromJSON)
    
newtype ReactorInput
  = ReactorAction (IO ())
  
--------------------------------------------------------------------------------
-- * conversion between LSP's and our positions

-- LSP starts from 0, Megaparsec starts from 1...
srcPosToPos :: SrcPos -> J.Position
srcPosToPos (SrcPos l c) = J.Position (l-1) (c-1)

posToSrcPos :: J.Position -> SrcPos
posToSrcPos (J.Position l c) = SrcPos (l+1) (c+1)

locToRange :: Location -> J.Range
locToRange (Location p1 p2) = J.Range (srcPosToPos p1) (srcPosToPos p2)

rangeToLoc :: J.Range -> Location
rangeToLoc (J.Range p1 p2) = Location (posToSrcPos p1) (posToSrcPos p2) 

--------------------------------------------------------------------------------

-- | send back diagnostics
sendDiags :: J.NormalizedUri -> Maybe Int -> [Diag] -> LspM Config ()
sendDiags fileUri version mydiags = do
  let diags = 
        [ J.Diagnostic 
            { _range    = locToRange loc        -- range 
            , _severity = Just severity         -- severity
            , _code     = Nothing               -- code
            , _source   = Just lspServerName    -- source
            , _message  = T.pack msg            -- message
            , _tags     = Nothing               -- tags
            , _relatedInformation = Nothing     -- related info
            }
        | Diag loc severity msg <- mydiags
        ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

--------------------------------------------------------------------------------
  
-- we call this when the document is first loaded or when it changes
updateDocumentLsp 
  :: (NFData result, SomeVersion ver, J.HasVersion textdoc ver, J.HasUri textdoc J.Uri) 
  => IDE result -> MVar (IDETable result) -> textdoc -> LspM Config ()
updateDocumentLsp ide global doc0 = do
  let doc      = J.toNormalizedUri (doc0 ^. J.uri)
      fileName = J.uriToFilePath   (doc0 ^. J.uri)
  -- liftIO $ debugM "updateDocumentLsp" $ "updating document " ++ show fileName
  mdoc <- getVirtualFile doc
  case mdoc of
    Just (VirtualFile lsp_ver file_ver str) -> do
      updateDocument ide global doc0 fileName (Rope.toText str)
    Nothing -> do
      liftIO $ debugM "updateDocumentLsp" $ "updateDocumentLsp: vfs returned Nothing"
      
--------------------------------------------------------------------------------
-- * the main event handler

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debugM "reactor" "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers 
  :: forall result. NFData result => IDE result -> MVar (IDETable result)
  -> TChan ReactorInput -> Handlers (LspM Config)
lspHandlers ide global rin = mapHandlers goReq goNot (handle ide global) where

  goReq :: forall (a :: J.Method J.FromClient J.Request). Handler (LspM Config) a -> Handler (LspM Config) a
  goReq f = \msg k -> do
    env <- getLspEnv
    liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

  goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
  goNot f = \msg -> do
    env <- getLspEnv
    liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

--------------------------------------------------------------------------------
-- * The actual event handling logic

-- | Where the actual logic resides for handling requests and notifications.
handle :: forall result. NFData result => IDE result -> MVar (IDETable result) -> Handlers (LspM Config)
handle ide global = mconcat
      
  -- See 
  -- <https://hackage.haskell.org/package/lsp-types-1.1.0.0/docs/Language-LSP-Types.html#t:MessageParams>
  -- for request parameter types, and
  -- <https://hackage.haskell.org/package/lsp-types-1.1.0.0/docs/Language-LSP-Types.html#t:ResponseResult>
  -- for the response types

  [ -- initialized notification   
    notificationHandler J.SInitialized $ \_notification -> do
      liftIO $ debugM "reactor.handle" $ "Initialized Notification"
      -- we could register extra capabilities here, but we are not doing that.
      return ()

    ----------------------------------------------------------------------------

    -- open document notification
  , notificationHandler J.STextDocumentDidOpen $ \notification -> do
      let doc = notification ^. J.params . J.textDocument 
      updateDocumentLsp ide global doc

    -- save document notification
  , notificationHandler J.STextDocumentDidSave $ \notification -> do
      -- let doc = notification ^. J.params . J.textDocument 
      return ()

    -- close document notification
  , notificationHandler J.STextDocumentDidClose $ \notification -> do
      let uri = notification ^. J.params . J.textDocument . J.uri
      closeDocument ide global (J.toNormalizedUri uri)
            
    -- change document notification
    -- we should re-parse and update everything!
  , notificationHandler J.STextDocumentDidChange $ \notification -> do
      let doc  = notification ^. J.params . J.textDocument 
      updateDocumentLsp ide global doc

    ----------------------------------------------------------------------------
    -- hover request         
    -- we should send back some tooltip info 

    -- MessageParams TextDocumentHover = HoverParams
  , requestHandler J.STextDocumentHover $ \req responder -> do
      let J.HoverParams tdoc pos _workdone = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ debugM "reactor.handle" $ "hover request at " ++ show (posToSrcPos pos)   
      mbMsg   <- ideGetMaybe uri $ \res -> ideOnHover ide res (posToSrcPos pos)
      mbHover <- case mbMsg of
        Nothing -> return Nothing
        Just (loc,msgs) -> do
          liftIO $  debugM "reactor.handle" $ "ide says: " ++ show msgs   
          let hc  = J.HoverContents $ J.markedUpContent lspServerName (T.pack $ unlines msgs)
              hov = J.Hover hc (Just $ locToRange loc) :: J.Hover
          return $ Just hov
      responder (Right mbHover)
      -- . ^^^ ResponseResult TextDocumentHover = Maybe Hover
      -- 
    ----------------------------------------------------------------------------
    -- highlight request
    -- we should send back a list of ranges
    
  , requestHandler J.STextDocumentDocumentHighlight $ \req responder -> do
      let J.DocumentHighlightParams tdoc pos _workdone _partialres = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ debugM "reactor.handle" $ "highlight request at " ++ show (posToSrcPos pos)   
      hlList1 <- ideGetList uri $ \res -> ideHighlight ide res (posToSrcPos pos)
      let hlList = [ J.DocumentHighlight (locToRange loc) Nothing | loc <- hlList1 ]
      responder (Right $ J.List hlList) 

    ----------------------------------------------------------------------------
    -- (jump to) definition request

  , requestHandler J.STextDocumentDefinition $ \req responder -> do
      let J.DefinitionParams tdoc pos _workdone _partialres = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ debugM "reactor.handle" $ "definition request at " ++ show (posToSrcPos pos)   
      mbloc <- ideGetMaybe uri $ \res -> ideDefinLoc ide res (posToSrcPos pos)
      let rsp :: J.Location |? (J.List J.Location |? J.List J.LocationLink)
          rsp = case mbloc of
            Just loc -> J.InL (J.Location doc (locToRange loc))
            Nothing  -> J.InR (J.InL $ J.List [])       -- ?? how to return failure - apparently this way
      responder (Right rsp)
    -- . ^^^ ResponseResult TextDocumentDefinition = Location |? (List Location |? List LocationLink)

    ----------------------------------------------------------------------------
    -- completion request

  , requestHandler J.STextDocumentCompletion $ \req responder -> do
      let J.CompletionParams tdoc pos _workdone _partialrestok _ctx  = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ debugM "reactor.handle" $ "completion request at " ++ show (posToSrcPos pos)   
      clist <- ideGetList uri $ \res -> ideCompletion ide res (posToSrcPos pos)
      -- liftIO $ debugM "reactor.handle" $ "completion list = " ++ show (map fst clist)   
      let items = map mkCompletionItem clist
      let rsp = J.InR (J.CompletionList False (J.List items))
      responder (Right rsp)
      -- . ^^^ ResponseResult TextDocumentCompletion = List CompletionItem |? CompletionList

    ----------------------------------------------------------------------------
    -- rename request

  , requestHandler J.STextDocumentRename $ \req responder -> do
      let J.RenameParams tdoc pos _workdone newName = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ debugM "reactor.handle" $ "rename request at " ++ show (posToSrcPos pos)   
      list <- ideGetList uri $ \res -> ideRename ide res (posToSrcPos pos) (T.unpack newName)
      -- liftIO $ debugM "reactor.handle" $ "renaming = " ++ show list
      let edits = J.List [ J.TextEdit (locToRange loc) (T.pack newText) | (loc,newText) <- list ]
      let vtdoc = J.VersionedTextDocumentIdentifier doc (Just 0) -- Nothing  -- ???
      let docedit = J.InL (J.TextDocumentEdit vtdoc edits) :: J.DocumentChange
      let rsp = J.WorkspaceEdit Nothing $ Just (J.List [docedit])
      responder (Right rsp)  
      -- . ^^^ ResponseResult TextDocumentRename = WorkspaceEdit


  ]

  where

    -- common access patterns
    ideGetMaybe :: J.NormalizedUri -> (result -> Maybe a) -> LspM config (Maybe a)
    ideGetMaybe uri user = liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
      Nothing    -> return Nothing
      Just table -> case Map.lookup uri table >>= ideResult of
        Nothing     -> return Nothing
        Just result -> return (user result)

    ideGetList :: J.NormalizedUri -> (result -> [a]) -> LspM config [a]
    ideGetList uri user = liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
      Nothing    -> return []
      Just table -> case Map.lookup uri table >>= ideResult of
        Nothing     -> return []
        Just result -> return (user result)


--------------------------------------------------------------------------------
      
syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just (J.InL False) 
  }

lspOptions :: Options
lspOptions = def 
  { textDocumentSync = Just syncOptions
--  , executeCommandCommands = Just ["lsp-hello-command"]
  }

--------------------------------------------------------------------------------

  