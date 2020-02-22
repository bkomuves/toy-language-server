
-- | This module connects the language implementation with the @haskell-lsp@
-- library

module Main where

--------------------------------------------------------------------------------

import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Language.Haskell.LSP.Types ( DiagnosticSeverity(..) )

import Common
import ToyLang
import LSP

--------------------------------------------------------------------------------

main = do
  lspMain myIDE

--------------------------------------------------------------------------------

myIDE :: IDE CheckResult
myIDE = IDE
  { ideCheckDocument = parseAndCheck "src" . T.unpack
  , ideDiagnostics   = myDiag
  , ideOnHover       = myHover
  , ideHighlight     = myHighlight
  , ideDefinLoc      = myDefinLoc
  }

-- error diagnostics
myDiag :: CheckResult -> [Diag]
myDiag (CheckResult messages nfos usage) = catMaybes $ map worker messages where
  worker (Located loc msg) = Just $ case msg of
    NotInScope  name     -> Diag loc DsError $ "variable " ++ quoted name ++ " not in scope"
    Shadowing   name loc -> Diag loc DsInfo  $ quoted name ++ " shadows earlier definition"
    NotAColor   col      -> Diag loc DsError $ quoted col ++ " is not a color"
    DeclInvalid name     -> Diag loc DsError $ "declaration " ++ quoted name ++ " is invalid"
    TypeError   text     -> Diag loc DsError text
    Warning     text     -> Diag loc DsWarning text
    ParseErr    text     -> Diag loc DsError $ "cannot parse: " ++ text        

-- type information when hovering        
myHover :: CheckResult -> SrcPos -> Maybe (Location,[String])
myHover (CheckResult message nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing         -> Nothing
    Just (loc,list) -> case catMaybes (map worker list) of
      [] -> Nothing
      ls -> Just (loc, ls) 
  where
    worker info = case info of
      HasType ty -> Just (prettyType ty)
      _          -> Nothing
      
-- highlight variable usage
myHighlight :: CheckResult -> SrcPos -> [Location]
myHighlight (CheckResult messages nfos usage) pos = case findInnerMost pos usage of
  Just (defloc,list) -> defloc:list
  Nothing -> case findInnerMost pos nfos of
    Nothing              -> [] 
    Just (thisloc,infos) -> case [ defloc | DefinedAt defloc <- infos ] of
      []         -> []
      (defloc:_) -> case Map.lookup defloc usage of
        Nothing    -> []
        Just list  -> (defloc:list)

-- find definition
myDefinLoc :: CheckResult -> SrcPos -> Maybe Location
myDefinLoc (CheckResult messages nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing              -> Nothing
    Just (thisloc,infos) -> case [ defloc | DefinedAt defloc <- infos ] of
      []         -> Nothing
      (defloc:_) -> Just defloc
      
--------------------------------------------------------------------------------
-- recall the definitions from ToyLang

{-
data CheckResult = CheckResult
  { chkMessages :: [Located Message]
  , chkInfo     :: Map Location [Info]
  , chkUsage    :: Map Location [Location]
  }
    
-- | Information collected during checking
data Info
  = DefinedAt !Location      -- ^ where was this variable defined
  | HasType   !Type          -- ^ what type this expression has 
  deriving Show

data Message
  = NotInScope  Name             -- ^ variable not in scope
  | Shadowing   Name Location    -- ^ new definitions shadows an already existing one
  | NotAColor   String           -- ^ color literal is not recognized
  | TypeError   String           -- ^ type checking error
  | DeclInvalid Name             -- ^ this declaration is invalid (for any reasons)
  | Warning     String           -- ^ a warning
  | ParseErr    String           -- ^ parse error
-}

--------------------------------------------------------------------------------
