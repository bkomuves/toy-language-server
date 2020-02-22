
-- | An simple abstraction layer above the rather complex 
-- and low-level @haskell-lsp@ library.

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
module LSP where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Reader
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception as E
import Control.Lens

import GHC.Generics (Generic)
import Control.DeepSeq

import Data.Default
import qualified Data.Text as T
import qualified Data.Rope.UTF16 as Rope

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Exit
import System.IO.Unsafe as Unsafe

import qualified Language.Haskell.LSP.Control     as CTRL
import qualified Language.Haskell.LSP.Core        as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types       as J
import qualified Language.Haskell.LSP.Types.Lens  as J
import           Language.Haskell.LSP.VFS

import qualified Language.Haskell.LSP.Utility     as U
import qualified System.Log.Logger                as L

import Common

--------------------------------------------------------------------------------

lspServerName :: T.Text
lspServerName = T.pack "toy-ide"

logging = True
logFile = "/tmp/toy-ide.log"
sessionLogFile = "/tmp/toy-ide-session.log"

--------------------------------------------------------------------------------

data IDE result = IDE
  { ideCheckDocument :: T.Text -> result
  , ideDiagnostics   :: result -> [Diag]
  , ideOnHover       :: result -> SrcPos -> Maybe (Location,[String])
  , ideHighlight     :: result -> SrcPos -> [Location]
  , ideDefinLoc      :: result -> SrcPos -> Maybe Location
  , ideCompletion    :: result -> SrcPos -> [(String, Maybe J.CompletionItemKind)]
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

adjustMVar :: MVar a -> (a -> a) -> IO ()
adjustMVar mv f = do
  x <- takeMVar mv
  putMVar mv $! f x

mapReplaceIfExists :: Ord k => k -> v -> Map k v -> Map k v 
mapReplaceIfExists !k !v = Map.alter f k where
  f Nothing  = Nothing
  f (Just _) = Just v

--------------------------------------------------------------------------------

defCompletionItem :: J.CompletionItem
defCompletionItem = J.CompletionItem
  { J._label = T.empty            -- The label of this completion item. By default also the text that is inserted when selecting this completion.
  , J._kind  = Nothing
  , J._detail = Nothing           -- A human-readable string with additional information about this item, like type or symbol information.
  , J._documentation = Nothing    -- A human-readable string that represents a doc-comment.
  , J._deprecated = Nothing        -- Indicates if this item is deprecated.
  , J._preselect = Nothing        -- Select this item when showing. *Note* that only one completion item can be selected and that the tool / client decides which item that is. The rule is that the *first* item of those that match best is selected.
  , J._sortText = Nothing         -- A string that should be used when filtering a set of completion items. When falsy the label is used.
  , J._filterText = Nothing            --  A string that should be used when filtering a set of completion items. When falsy the label is used.
  , J._insertText = Nothing            --  A string that should be inserted a document when selecting this completion. When falsy the label is used.
  , J._insertTextFormat = Nothing      --  The format of the insert text. The format applies to both the insertText property and the newText property of a provided textEdit.
  , J._textEdit = Nothing              --  An edit which is applied to a document when selecting this completion. When an edit is provided the value of insertText is ignored.
  , J._additionalTextEdits = Nothing   --  An optional array of additional text edits that are applied when selecting this completion. Edits must not overlap with the main edit nor with themselves.
  , J._commitCharacters = Nothing      --  An optional set of characters that when pressed while this completion is active will accept it first and then type that character. *Note* that all commit characters should have `length=1` and that superfluous characters will be ignored.
  , J._command = Nothing               --  An optional command that is executed *after* inserting this completion. *Note* that additional modifications to the current document should be described with the additionalTextEdits-property.
  , J._xdata = Nothing 
  }
  
mkCompletionItem :: String -> Maybe J.CompletionItemKind -> J.CompletionItem
mkCompletionItem s mbkind = defCompletionItem
  { J._label = T.pack s   
  , J._kind  = mbkind
  }

--------------------------------------------------------------------------------

-- why the fuck do i need this shit ?!
class SomeVersion v where versionToMaybeInt :: v -> Maybe Int
instance SomeVersion (Maybe Int) where versionToMaybeInt = id
instance SomeVersion Int         where versionToMaybeInt = Just
  
checkDocument 
  :: (NFData result, SomeVersion ver, J.HasUri textdoc J.Uri, J.HasVersion textdoc ver)
  => IDE result -> MVar (IDETable result) 
  -> textdoc -> Maybe FilePath -> T.Text -> R () ()
checkDocument ide global tdoc mbFilePath text = do
  let uri = tdoc ^. J.uri . to J.toNormalizedUri
      ver = tdoc ^. J.version
  liftIO $ U.logs "checking document..."
  let !result = ideCheckDocument ide text
  -- liftIO $ print result
  rnf result `seq` (liftIO $ adjustMVar global (mapReplaceIfExists uri (IdeChecked result)))
  liftIO $ U.logs "computing diagnostics..."
  let !diags = ideDiagnostics ide result
  sendDiags uri (versionToMaybeInt ver) diags    -- version ???
  rnf diags `seq` (liftIO $ adjustMVar global (mapReplaceIfExists uri (IdeDone result)))
  liftIO $ U.logs "done."
  return ()
   
updateDocument 
  :: (NFData result, SomeVersion ver, J.HasUri textdoc J.Uri, J.HasVersion textdoc ver)
  => IDE result -> MVar (IDETable result) 
  -> textdoc -> Maybe FilePath -> T.Text -> R () ()
updateDocument ide global tdoc mbFileName text = do
  let uri = tdoc ^. J.uri . to J.toNormalizedUri
  lf <- ask
  liftIO $ U.logs "updating document..."
  liftIO $ do
    table <- takeMVar global
    case Map.lookup uri table of
      Just (IdeChecking old_threadid) -> killThread old_threadid
      _ -> return () 
    threadid <- forkIO $ flip runReaderT lf $ checkDocument ide global tdoc mbFileName text
    putMVar global $! (Map.insert uri (IdeChecking threadid) table)

closeDocument 
  :: NFData result => IDE result -> MVar (IDETable result) 
  -> J.NormalizedUri  -> R () ()
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
   
run  :: NFData result => IDE result -> MVar (IDETable result) -> IO Int
run ide global = flip E.catches handlers $ do    
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  let dispatcher lf = forkIO (reactor ide global lf rin) >> return Nothing
  let iniCallbacks = Core.InitializeCallbacks
        { Core.onInitialConfiguration = \_ -> Right ()
        , Core.onConfigurationChange  = \_ -> Right ()
        , Core.onStartup = dispatcher
        }
  flip E.finally finalProc $ do
    when logging $ Core.setupLogger (Just logFile) [] L.DEBUG
    let session_log = if logging then Just sessionLogFile else Nothing
    CTRL.run iniCallbacks (lspHandlers rin) lspOptions session_log    
  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = return () -- L.removeAllHandlers
    ioExcept   (e :: E.IOException  ) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1      

--------------------------------------------------------------------------------
    
data ReactorInput  
  = HandlerRequest FromClientMessage    
  deriving Show
  
type R c a = ReaderT (Core.LspFuncs c) IO a  

--------------------------------------------------------------------------------

reactorSend :: FromServerMessage -> R () ()
reactorSend msg = do
  lf <- ask
  liftIO $ Core.sendFunc lf msg

publishDiagnostics :: Int -> J.NormalizedUri -> J.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

nextLspReqId :: R () J.LspId
nextLspReqId = do
  f <- asks Core.getNextReqId
  liftIO f
  
--------------------------------------------------------------------------------
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

sendDiags :: J.NormalizedUri -> Maybe Int -> [Diag] -> R () ()
sendDiags fileUri version mydiags = do
  let diags = 
        [ J.Diagnostic 
            (locToRange loc)      -- range 
            (Just severity)       -- severity
            Nothing               -- code
            (Just lspServerName)  -- source
            (T.pack msg)          -- message
            (Just (J.List []))    -- related info
        | Diag loc severity msg <- mydiags
        ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)
  
-- we call this when the document is first loaded or when it changes
-- updateDocumentLsp :: Core.LspFuncs () -> ? -> ?
updateDocumentLsp ide global lf doc0 = do
  let doc      = J.toNormalizedUri (doc0 ^. J.uri)
      fileName = J.uriToFilePath   (doc0 ^. J.uri)
  -- liftIO $ U.logs $ "updating document " ++ show fileName
  mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
  case mdoc of
    Just (VirtualFile lsp_ver file_ver str) -> do
      updateDocument ide global doc0 fileName (Rope.toText str)
    Nothing -> do
      liftIO $ U.logs $ "updateDocumentLsp: vfs returned Nothing"
      
--------------------------------------------------------------------------------

reactor 
  :: NFData result => IDE result -> MVar (IDETable result) 
  -> Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor ide global lf inp = flip runReaderT lf $ forever $ do
  inval <- liftIO $ atomically $ readTChan inp
  case inval of
  
    -- response from client   
    HandlerRequest (RspFromClient rm) -> do
      liftIO $ U.logs $ "got RspFromClient:" ++ show rm
      return ()
      
    -- initialized notification   
    HandlerRequest (NotInitialized _notification) -> do
      liftIO $ U.logs $ "Initialized Notification"
      -- we could register extra capabilities here, but we are not doing that.
      return ()

    ----------------------------------------------------------------------------

    -- open document notification
    HandlerRequest (NotDidOpenTextDocument notification) -> do
      let doc = notification ^. J.params . J.textDocument -- . J.uri
      updateDocumentLsp ide global lf doc
      return ()

    -- save document notification
    HandlerRequest (NotDidSaveTextDocument notification) -> do
      let doc = notification ^. J.params . J.textDocument -- . J.uri
--      sendDiags (J.toNormalizedUri (doc ^. J.uri)) Nothing  
--        [Diag (Location (SrcPos 2 2) (SrcPos 2 2)) J.DsError "fuck this shit / save"]
      return ()

    -- close document notification
    HandlerRequest (NotDidCloseTextDocument notification) -> do
      let doc = notification ^. J.params . J.textDocument . J.uri
      closeDocument ide global (J.toNormalizedUri doc)
      return ()
            
    -- change document notification
    -- we should re-parse and update everything!
    HandlerRequest (NotDidChangeTextDocument notification) -> do
      let doc  = notification ^. J.params . J.textDocument -- . J.uri 
      updateDocumentLsp ide global lf doc
      return ()

    ----------------------------------------------------------------------------
    -- hover request         
    -- we should send back some tooltip info 

    HandlerRequest (ReqHover req) -> do
      let J.TextDocumentPositionParams tdoc pos _workdone = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ U.logs $ "hover request at " ++ show (posToSrcPos pos)   
      mbHover <- liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
        Nothing    -> return Nothing
        Just table -> case Map.lookup uri table >>= ideResult of
          Nothing     -> return Nothing
          Just result -> do
            let  mbMsg = ideOnHover ide result (posToSrcPos pos)
            case mbMsg of
              Nothing -> return Nothing
              Just (loc,msgs) -> do
                liftIO $ U.logs $ "ide says: " ++ show msgs   
                let ms = J.HoverContents $ J.markedUpContent lspServerName (T.pack $ unlines msgs)
                    ht = J.Hover ms (Just $ locToRange loc)
                return $ Just ht
      reactorSend $ RspHover $ Core.makeResponseMessage req mbHover 
        
    ----------------------------------------------------------------------------
    -- highlight request
    -- we should send back a list of ranges
    
    HandlerRequest (ReqDocumentHighlights req) -> do
      let J.TextDocumentPositionParams tdoc pos _workdone = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ U.logs $ "highlight request at " ++ show (posToSrcPos pos)   
      hlList <- liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
        Nothing    -> return []
        Just table -> case Map.lookup uri table >>= ideResult of
          Nothing     -> return []
          Just result -> do
            let hlList1 = ideHighlight ide result (posToSrcPos pos)
            return [ J.DocumentHighlight (locToRange loc) Nothing | loc <- hlList1 ]
      reactorSend $ RspDocumentHighlights $ Core.makeResponseMessage req (J.List hlList) 

    ----------------------------------------------------------------------------
    -- (jump to) definition request

    HandlerRequest (ReqDefinition req) -> do
      let J.TextDocumentPositionParams tdoc pos _workdone = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ U.logs $ "definition request at " ++ show (posToSrcPos pos)   
      mbloc <- liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
        Nothing    -> return Nothing
        Just table -> case Map.lookup uri table >>= ideResult of
          Nothing     -> return Nothing
          Just result -> return $ ideDefinLoc ide result (posToSrcPos pos)
      reactorSend $ RspDefinition $ Core.makeResponseMessage req $ case mbloc of
        Just loc -> J.SingleLoc (J.Location doc (locToRange loc))
        Nothing  -> J.MultiLoc []    -- ?? how to return failure

    ----------------------------------------------------------------------------
    -- completion request

    HandlerRequest (ReqCompletion req) -> do
      let J.CompletionParams tdoc pos _ctx _workdone = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ U.logs $ "completion request at " ++ show (posToSrcPos pos)   
      clist <- liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
        Nothing    -> return []
        Just table -> case Map.lookup uri table >>= ideResult of
          Nothing     -> return []
          Just result -> return $ ideCompletion ide result (posToSrcPos pos)
      -- liftIO $ U.logs $ "completion list = " ++ show (map fst clist)   
      let items = [ mkCompletionItem label mbkind | (label,mbkind) <- clist ]
      reactorSend $ RspCompletion $ Core.makeResponseMessage req 
        $ J.CompletionList $ J.CompletionListType False (J.List items)

    ----------------------------------------------------------------------------
    -- rename request

    HandlerRequest (ReqRename req) -> do
      let J.RenameParams tdoc pos newName _workdone = req ^. J.params
          doc = req ^. J.params . J.textDocument . J.uri 
          uri = J.toNormalizedUri doc
      liftIO $ U.logs $ "rename request at " ++ show (posToSrcPos pos)   
      list <- liftIO (tryReadMVar global) >>= \mbtable -> case mbtable of
        Nothing    -> return []
        Just table -> case Map.lookup uri table >>= ideResult of
          Nothing     -> return []
          Just result -> return $ ideRename ide result (posToSrcPos pos) (T.unpack newName)
      liftIO $ U.logs $ "renaming = " ++ show list
      let edits = J.List [ J.TextEdit (locToRange loc) (T.pack newText) | (loc,newText) <- list ]
      let vtdoc = J.VersionedTextDocumentIdentifier doc (Just 0) -- Nothing  -- ???
      let docedit = J.TextDocumentEdit vtdoc edits   
      reactorSend $ RspRename $ Core.makeResponseMessage req 
        $ J.WorkspaceEdit Nothing $ Just (J.List [docedit])
        
    ----------------------------------------------------------------------------

    -- something unhandled above
    HandlerRequest om -> do
      liftIO $ U.logs $ "got HandlerRequest:" ++ show om
      return ()

--------------------------------------------------------------------------------
      
syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

lspOptions :: Core.Options
lspOptions = def 
  { Core.textDocumentSync = Just syncOptions
--  , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-hello-command"]))
  }

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin = def 
  { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
  , Core.responseHandler                          = Just $ responseHandlerCb rin
  , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
    --------------------------------------------
  , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
  , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
  , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
  , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
    ---------------------------------------------
  , Core.hoverHandler                             = Just $ passHandler rin ReqHover
  , Core.documentHighlightHandler                 = Just $ passHandler rin ReqDocumentHighlights
  , Core.definitionHandler                        = Just $ passHandler rin ReqDefinition
  , Core.completionHandler                        = Just $ passHandler rin ReqCompletion
  , Core.renameHandler                            = Just $ passHandler rin ReqRename
--  , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
--  , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
  }
      
--------------------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "got ResponseMessage, ignoring: " ++ show resp      
  return ()
 
--------------------------------------------------------------------------------

  