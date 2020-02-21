
-- | An simple abstraction layer above the rather complex 
-- and low-level @haskell-lsp@ library.

{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.Default
import qualified Data.Text as T
import qualified Data.Rope.UTF16 as Rope

import System.Exit
import System.IO.Unsafe as Unsafe

import qualified Language.Haskell.LSP.Control     as CTRL
import qualified Language.Haskell.LSP.Core        as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types       as J
import qualified Language.Haskell.LSP.Types.Lens  as J
-- import qualified Language.Haskell.LSP.Utility     as U
import           Language.Haskell.LSP.VFS


--------------------------------------------------------------------------------

data IDEState result
  = IdeChecking !ThreadId    -- ^ we are in the process of checking the source
  | IdeChecked  !result      -- ^ checking is done, but no feedback sent yet
  | IdeDone     !result      -- ^ feedback was also sent
  
{-# NOINLINE theGlobalState #-}
theGlobalState :: MVar (Map J.NormalizedUri IDEState)
theGlobalState = Unsafe.unsafePerformIO $ newMVar Map.empty

adjustMVar :: MVar a -> (a -> a) -> IO ()
adjustMVar mv f = do
  x <- takeMVar mv
  putMVar mv $! f x

--------------------------------------------------------------------------------

data IDE result = IDE
  { ideCheckDocument :: T.Text -> result
  , ideDiagnostics   :: result -> IO [Diag]
  , ideOnHover       :: SrcPos -> IO (Maybe String)
  }

data Diag = Diag
  { diagSeverity :: !DiagnosticSeverity
  , diagLocation :: !Location
  , diagMessage  :: !String
  }
  
checkDocument :: J.NormalizedUri -> Maybe FilePath -> T.Text -> IO ()
checkDocument = do
  ...
   
updateDocument :: J.NormalizedUri -> Maybe FilePath -> T.Text -> IO ()
updateDocument uri mbFileName text = do
  table <- takeMVar theGlobalState
  case Map.lookup uri table of
    Just (IdeChecking threadid) -> killThread threadid
    _ -> return () 
  threadid <- forkIO $ checkDocument uri mbFilePath text
  putMVar $! theGlobalState (Map.insert (IdeChecking threadid))

closeDocument :: J.NormalizedUri  -> IO ()
closeDocument uri = adjustMVar theGlobalState (Map.delete uri)
  
--------------------------------------------------------------------------------
-- * interfacing with @haskell-lsp@

lspMain :: IO ()
lspMain = do
  run >>= \r -> case r of
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
   
run :: IO Int
run = flip E.catches handlers $ do    
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  let dispatcher lf = forkIO (reactor lf rin) >> return Nothing
  let iniCallbacks = Core.InitializeCallbacks
        { Core.onInitialConfiguration = \_ -> Right ()
        , Core.onConfigurationChange  = \_ -> Right ()
        , Core.onStartup = dispatcher
        }
  flip E.finally finalProc $ do
    CTRL.run iniCallbacks (lspHandlers rin) lspOptions Nothing    
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

-- we call this when the document is first loaded or when it changes
-- updateDocumentLsp :: Core.LspFuncs () -> ? -> ?
updateDocumentLsp lf doc0 = do
  let doc      = J.toNormalizedUri doc0
      fileName = J.uriToFilePath   doc0
  -- liftIO $ putStrLn $ "updating document " ++ show fileName
  mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
  case mdoc of
    Just (VirtualFile lsp_ver file_ver str) -> do
      liftIO $ updateDocument doc fileName (Rope.toText str)
    Nothing -> do
      liftIO $ putStrLn $ "updateDocumentLsp: vfs returned Nothing"
      
--------------------------------------------------------------------------------

reactor :: Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp = flip runReaderT lf $ forever $ do
  inval <- liftIO $ atomically $ readTChan inp
  case inval of
  
    -- response from client   
    HandlerRequest (RspFromClient rm) -> do
      liftIO $ putStrLn $ "got RspFromClient:" ++ show rm
      return ()
      
    -- initialized notification   
    HandlerRequest (NotInitialized _notification) -> do
      liftIO $ putStrLn $ "Initialized Notification"
      -- we could register extra capabilities here, but we are not doing that.
      return ()

    ----------------------------------------------------------------------------

    -- open document notification
    HandlerRequest (NotDidOpenTextDocument notification) -> do
      let doc = notification ^. J.params . J.textDocument . J.uri
      updateDocumentLsp lf doc

    -- save document notification
    HandlerRequest (NotDidSaveTextDocument notification) -> do
      let doc = notification ^. J.params . J.textDocument . J.uri
      return ()

    -- close document notification
    HandlerRequest (NotDidCloseTextDocument notification) -> do
      let doc = notification ^. J.params . J.textDocument . J.uri
      closeDocument (J.toNormalizedUri doc)
            
    -- change document notification
    -- we should re-parse and update everything!
    HandlerRequest (NotDidChangeTextDocument notification) -> do
      let doc  = notification ^. J.params . J.textDocument . J.uri 
      updateDocumentLsp lf doc

    ----------------------------------------------------------------------------

    -- hover request     
    -- we should send back some tooltip info 
    HandlerRequest (ReqHover req) -> do
      let J.TextDocumentPositionParams _doc pos _workdone = req ^. J.params
          J.Position line col = pos
      let ht = Just $ J.Hover ms (Just range)
          ms = J.HoverContents $ J.markedUpContent (T.pack "lsp-hello") (T.pack $ "TYPE INFO @ " ++ show line ++ ":" ++ show col)
          range = J.Range pos pos
      reactorSend $ RspHover $ Core.makeResponseMessage req ht

    ----------------------------------------------------------------------------

    -- something unhandled above
    HandlerRequest om -> do
      liftIO $ putStrLn $ "got HandlerRequest:" ++ show om
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
  , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
  , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
  , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
  , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
  , Core.responseHandler                          = Just $ responseHandlerCb rin
    ---------------------------------------------
--  , Core.renameHandler                            = Just $ passHandler rin ReqRename
  , Core.hoverHandler                             = Just $ passHandler rin ReqHover
--  , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
--  , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
--  , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
  }
      
--------------------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  putStrLn $ "got ResponseMessage, ignoring: " ++ show resp      
  return ()
 
--------------------------------------------------------------------------------

  