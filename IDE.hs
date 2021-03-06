
-- | This module connects the language implementation with 
-- the abstraction layer built on the top of the @haskell-lsp@ library

module Main where

--------------------------------------------------------------------------------

import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Language.LSP.Types ( DiagnosticSeverity(..) , CompletionItemKind(..) )

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
  , ideGetToken      = myGetToken
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

-- find token at location (only used for debugging)
myGetToken :: CheckResult -> SrcPos -> Maybe (Location,String)
myGetToken (CheckResult message nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing         -> Nothing
    Just (loc,list) -> case [ tok | NfoToken tok <- list ] of 
      []      -> Nothing
      (tok:_) -> Just (loc,tok)

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
myCompletion :: CheckResult -> SrcPos -> [IDECompletion]
myCompletion (CheckResult messages nfos usage) pos = 
  case findInnerMost pos nfos of
    Nothing              -> []
    Just (thisloc,infos) -> colorCompl thisloc infos ++ greekCompl thisloc infos 

  where

    mbHead :: [a] -> Maybe a
    mbHead (x:xs) = Just x
    mbHead []     = Nothing

{-
    colorCompl infos = case [ () | HasType (ColorT) <- infos ] of
      []    -> []
      (_:_) -> case [ tok | NfoToken tok <- infos ] of
        []      -> []
        (tok:_) -> case mbHead tok of
          Just '#'  -> [ (color, Just CiColor) | color <- completeColor (tail tok) ]
          _         -> []
-}

    colorCompl loc infos = case [ tok | NfoToken tok <- infos ] of
      []      -> []
      (tok:_) -> case mbHead tok of
        Just '#'  -> [ IDECompletion color (Just ('#':color)) (SpecCompletion loc) (Just CiColor) | color <- completeColor (tail tok) ]
        _         -> []

    greekCompl loc infos = case [ tok | NfoToken tok <- infos ] of
      []      -> []
      (tok:_) -> case mbHead tok of
        Just '\\' -> [ IDECompletion label (Just replace) (SpecCompletion loc) (Just CiVariable) | (label,replace) <- completeGreek (tail tok) ]
        _         -> []

--------------------------------------------------------------------------------
-- recall the definitions from ToyLang

{-
data CheckResult = CheckResult
  { chkMessages :: [Located Message]         -- ^ error messages with location
  , chkInfo     :: Map Location [Info]       -- ^ other information based on location
  , chkUsage    :: Map Location [Location]   -- ^ where is the variable we bind here used?
  }
    
-- | Information collected during checking
data Info
  = DefinedAt !Location          -- ^ where was this variable defined
  | HasType   !Type              -- ^ what type this expression has 
  | NfoToken  !String            -- ^ the underlying text
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
