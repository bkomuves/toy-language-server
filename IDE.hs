
-- | This module connects the language implementation with 
-- the [abstraction layer on the top of the] @haskell-lsp@ library

module Main where

--------------------------------------------------------------------------------

import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Language.Haskell.LSP.Types ( DiagnosticSeverity(..) , CompletionItemKind(..) )

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
  , ideCompletion    = myCompletion
  , ideRename        = myRename
  }

-- error diagnostics
myDiag :: CheckResult -> [Diag]
myDiag (CheckResult messages nfos usage) = catMaybes $ map worker messages where
  worker (Located loc msg) = Just $ case msg of
    NotInScope  name      -> Diag loc DsError $ "variable " ++ quoted name ++ " not in scope"
    Shadowing   name ploc -> Diag loc DsInfo  $ quoted name ++ " shadows earlier definition at " ++ prettyLoc ploc
    NotAColor   col       -> Diag loc DsError $ quoted col ++ " is not a color"
    DeclInvalid name      -> Diag loc DsError $ "declaration " ++ quoted name ++ " is invalid"
    TypeError   text      -> Diag loc DsError text
    Warning     text      -> Diag loc DsWarning text
    ParseErr    text      -> Diag loc DsError $ "cannot parse: " ++ text        

-- type information when hovering        
myHover :: CheckResult -> SrcPos -> Maybe (Location,[String])
myHover (CheckResult message nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing         -> Nothing
    Just (loc,list) -> 
      let mbtoken = case [ tok | NfoToken tok <- list ] of 
            []      -> Nothing
            (tok:_) -> Just tok
      in  case catMaybes (map (worker mbtoken) list) of
            [] -> Nothing
            ls -> Just (loc, ls) 
  where
    worker mbtoken info = case info of
      HasType ty -> Just $ maybe "" id mbtoken ++ " :: " ++ prettyType ty
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

-- globally rename variable 
myRename :: CheckResult -> SrcPos -> String -> [(Location,String)]
myRename checkResult pos newname = 
  [ (l,newname) | l <- myHighlight checkResult pos ]
  
-- find definition
myDefinLoc :: CheckResult -> SrcPos -> Maybe Location
myDefinLoc (CheckResult messages nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing              -> Nothing
    Just (thisloc,infos) -> case [ defloc | DefinedAt defloc <- infos ] of
      []         -> Nothing
      (defloc:_) -> Just defloc
      
-- completion
-- NB: completion apparently cannot start with special characters like `#`...
myCompletion :: CheckResult -> SrcPos -> [(String,Maybe CompletionItemKind)]
myCompletion (CheckResult messages nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing              -> []
    Just (thisloc,infos) -> case [ () | HasType (ColorT) <- infos ] of
      []    -> []
      (_:_) -> case [ tok | NfoToken tok <- infos ] of
        []      -> []
        (tok:_) -> [ (color, Just CiColor) | color <- completeColor (tail tok) ]
         
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
