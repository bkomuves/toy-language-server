
{-# LANGUAGE BangPatterns, PatternSynonyms, DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
module ToyLang where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.State.Strict

import Data.Void
import Data.Either
import Data.Char
import Data.List

import qualified Data.Map as Map ; import Data.Map (Map)

import GHC.Generics (Generic)
import Control.DeepSeq

import Text.Megaparsec hiding ( State )
import Text.Megaparsec.Char

import Common
  
--------------------------------------------------------------------------------
-- * the AST

type Name = String

data Type 
  = BoolT
  | NatT
  | ColorT
  deriving (Eq,Show,Generic,NFData)

prettyType :: Type -> String
prettyType ty = case ty of
  BoolT  -> "Bool"
  NatT   -> "Nat"
  ColorT -> "Color"

data Literal 
  = BoolLit  !Bool
  | NatLit   !Integer
  | ColorLit !String
  deriving Show

literalType :: Literal -> Type
literalType lit = case lit of
  BoolLit  {} -> BoolT
  NatLit   {} -> NatT
  ColorLit {} -> ColorT
  
type LExpr = Located Expr

data Expr 
  = LitE Literal
  | VarE Name
  | AddE LExpr LExpr
  deriving Show
      
data Decl = Decl 
  { declName :: Located Name
  , declType :: Located Type
  , declBody :: LExpr 
  }
  deriving Show

type Program = Map Name Decl

--------------------------------------------------------------------------------
-- * colors

knownColors :: [String]
knownColors = 
  [ "black"
  , "white"
  , "red"
  , "green"
  , "blue"
  , "yellow"
  , "magenta"
  , "cyan"
  , "rainbow"
  ]

--------------------------------------------------------------------------------
-- * check the whole document

data CheckResult = CheckResult
  { chkMessages :: [Located Message]
  , chkInfo     :: Map Location [Info]
  , chkUsage    :: Map Location [Location]
  }
  deriving (Show,Generic,NFData)
  
computeCheckResult :: [Located Message] -> Map Location [Info] -> CheckResult
computeCheckResult messages info = CheckResult messages info usage where
  usage = foldl' f Map.empty (Map.toList info) 
  f !old (useloc, infos) = foldl' (h useloc) old infos
  h !useloc old info = case info of
      DefinedAt defloc -> Map.alter (g useloc) defloc old
      _ -> old
  g !loc Nothing     = Just [loc] 
  g !loc (Just locs) = Just (loc:locs)
  
parseAndCheck :: FilePath -> String -> CheckResult
parseAndCheck fname text = computeCheckResult (parseErrs ++ reverse messages) info where
  ei_list   = parseSrc fname text
  parseErrs = [ Located (Location src src) (ParseErr msg) | (src,msg) <- lefts ei_list ]
  decls     = rights ei_list
  CheckState messages info 
    = execState (checkDeclList emptyScope decls) emptyCheckState

--------------------------------------------------------------------------------
-- * scope \/ type checker

-- | Error messages collected during checking
data Message
  = NotInScope  Name             -- ^ variable not in scope
  | Shadowing   Name Location    -- ^ new definitions shadows an already existing one
  | NotAColor   String           -- ^ color literal is not recognized
  | TypeError   String           -- ^ type checking error
  | DeclInvalid Name             -- ^ this declaration is invalid (for any reasons)
  | Warning     String           -- ^ a warning
  | ParseErr    String           -- ^ parse error
  deriving (Show,Generic,NFData)
  
-- | Information collected during checking
data Info
  = DefinedAt !Location      -- ^ where was this variable defined
  | HasType   !Type          -- ^ what type this expression has 
  deriving (Show,Generic,NFData)

data CheckState = CheckState 
  { messages :: ![Located Message]
  , info     :: !(Map Location [Info])
  }
  deriving (Show,Generic,NFData)

emptyCheckState :: CheckState
emptyCheckState = CheckState [] Map.empty
  
type CheckM a = State CheckState a

addMessage :: Location -> Message -> CheckM ()
addMessage loc msg = modify $ \st -> st { messages = Located loc msg : messages st }

mapInsert :: Ord k => k -> v -> Map k [v] -> Map k [v]
mapInsert k v map = case Map.lookup k map of
  Nothing -> Map.insert k [v]    map
  Just vs -> Map.insert k (v:vs) map
   
addInfo :: Location -> Info -> CheckM ()
addInfo loc nfo = modify $ \st -> st { info = mapInsert loc nfo (info st) }

type Scope = Map Name (Located Type)

emptyScope :: Scope
emptyScope = Map.empty

checkDeclList :: Scope -> [Decl] -> CheckM Scope
checkDeclList scope []     = return scope
checkDeclList scope (d:ds) = do
  (scope', _) <- checkDecl scope d
  checkDeclList scope' ds
   
checkDecl :: Scope -> Decl -> CheckM (Scope, Maybe Type)
checkDecl scope (Decl (Located loc name) (Located _ ty) body) = do
  mbty <- checkExpr scope body
  if mbty == Just ty
    then addInfo loc (HasType ty)
    else do
      addMessage loc (DeclInvalid name)
      case mbty of
        Nothing  -> nop
        Just ty' -> addMessage loc $ TypeError $ 
          ("cannot match inferred type " ++ prettyType ty' ++ " with declared type " ++ prettyType ty)  
  -- we add it to the scope anyway, as later code can refer to it
  let scope' = Map.insert name (Located loc ty) scope
  return (scope', mbty)
  
checkExpr :: Scope -> LExpr -> CheckM (Maybe Type)
checkExpr = go where

  go scope (Located loc expr)  = do
    mbty <- go1 scope loc expr
    case mbty of
      Just ty -> addInfo loc $ HasType ty
      Nothing -> nop
    return mbty
      
  go1 !scope !loc expr = case expr of

    VarE name -> case Map.lookup name scope of
      Nothing   -> do
        addMessage loc (NotInScope name)
        return Nothing
      Just (Located defloc ty) -> do
        addInfo loc (DefinedAt defloc)
        return (Just ty)
    
    LitE lit -> do
      case lit of 
        NatLit n -> do
          when (n == 0  ) $ addMessage loc (Warning "go back to school, zero is not a number!!!")
          when (n > 1000) $ addMessage loc (Warning "numbers bigger than 1000 do not exist!")
        ColorLit col -> do
          unless (elem col knownColors) $ addMessage loc (NotAColor col) 
          when   (col == "rainbow")     $ addMessage loc (Warning "rainbow is not really a color!")
        _ -> nop
      return (Just $ literalType lit)

    AddE e1 e2 -> do
      mbty1 <- go scope e1 
      mbty2 <- go scope e2
      case (mbty1 , mbty2) of
        (Just ty1 , Just ty2) -> if (ty1 == NatT && ty2 == NatT) 
          then return (Just NatT)
          else do
            addMessage loc (TypeError "arguments of `+` should have type Nat")
            return Nothing
        (_ , _) -> return Nothing
        
--------------------------------------------------------------------------------
-- * parsing

type ErrMsg = String

type Parser a = Parsec Void String a
  
newlineP :: Parser ()
newlineP = void (char '\n') <|> try (void (string "\r\n")) <|> void (char '\r')

spacesOnly :: Parser ()
spacesOnly = skipMany $ satisfy $ \c -> isSpace c && not (elem c "\n\r")

notNewline :: Parser ()
notNewline = void $ satisfy $ \c -> not (elem c "\n\r")

skipForward :: Parser ()
skipForward = skipMany notNewline >> newlineP

withPostSpace :: Parser a -> Parser a
withPostSpace p = do
  y <- p
  spacesOnly
  return y
  
eol1 :: Parser ()
eol1 = try $ do
  spacesOnly
  newlineP

eols :: Parser ()
eols = skipSome eol1
  
inParens :: Parser a -> Parser a
inParens p = do
  char '('
  space
  y <- p
  space
  char ')'
  return y

located :: Parser a -> Parser (Located a)
located parser = do
  a <- toSrcPos <$> getSourcePos
  y <- parser
  b <- toSrcPos <$> getSourcePos
  return (Located (Location a b) y)

--------------------------------------------------------------------------------
-- * the language parser

identP :: Parser String
identP = do
  x  <- letterChar <|> char '_'
  xs <- many (alphaNumChar <|> char '_')
  return (x:xs)

boolP :: Parser Bool
boolP = try 
  (   (string "True"  >> return True ) 
  <|> (string "False" >> return False) 
  <?> "a boolean"
  )
  
natP :: Parser Integer
natP = read <$> some digitChar 

colorP :: Parser String
colorP = do
  char '#'
  cs <- some letterChar
  return cs  
  
literalP :: Parser Literal
literalP 
  =   NatLit   <$> natP
  <|> ColorLit <$> colorP
  <|> BoolLit  <$> boolP
  <?> "a literal constant"

litP :: Parser Expr
litP = LitE <$> literalP

varP :: Parser Expr
varP = VarE <$> identP

atomicExprP :: Parser Expr
atomicExprP = (inParens exprP <|> litP <|> varP)

atomicLExprP :: Parser LExpr
atomicLExprP = located atomicExprP

exprP :: Parser Expr
exprP = forgetLocation <$> lexprP

lexprP :: Parser LExpr
lexprP = 
  do
    e  <- withPostSpace atomicLExprP 
    es <- many (char '+' >> space >> withPostSpace atomicLExprP)
    return $ foldl1 lAddE (e:es) 
  where    
    lAddE :: LExpr -> LExpr -> LExpr
    lAddE e1@(Located loc1 _) e2@(Located loc2 _) = Located (locSpan loc1 loc2) (AddE e1 e2)
    
atomicTypeP :: Parser Type
atomicTypeP = try 
  (   (string "Bool"  >> return BoolT ) 
  <|> (string "Nat"   >> return NatT  ) 
  <|> (string "Color" >> return ColorT) 
  <?> "a type"
  )
  
typeP :: Parser Type
typeP = atomicTypeP

declP :: Parser Decl
declP = do
  name <- located (identP <?> "an identifier")
  space >> char ':' >> space
  ty <- located (typeP <?> "a type")
  space >> char '=' >> space
  rhs <- lexprP <?> "an expression"
  eols
  return (Decl name ty rhs)

-- we have to recover from failure
declWithRecoveryP :: Parser (Either (SrcPos,ErrMsg) Decl)
declWithRecoveryP = do
  flip withRecovery (Right <$> declP) $ \err -> do
    pos <- getSourcePos
    let msg = parseErrorTextPretty err
    -- start again from the next line
    skipForward                             
    return (Left (toSrcPos pos , msg))

type MyParseErr = (SrcPos, ErrMsg)    

declListP :: Parser [Either MyParseErr Decl]
declListP = do
  skipMany eol1
  ds <- many declWithRecoveryP
  space 
  eof
  return ds

parseSrc :: FilePath -> String -> [Either MyParseErr Decl]
parseSrc fname text = case runParser declListP fname (text ++ "\n") of
  Left  err -> [Left (SrcPos 1 1, "FATAL ERROR: parsing failed completely")] 
  Right eis -> eis 

--------------------------------------------------------------------------------
-- * example source code
  
exampleSrc :: String
exampleSrc = unlines
  [ "the_truth  : Bool  = True"
  , "the_lie    : Bool  = the_truth"
  , ""
  , "one        : Nat   = 1"
  , "fiftyfive  : Nat   = 50 + 4 + one"
  , "too_big    : Nat   = 12345"
  , "bad_number : Nat   = (((666)))"
  , "" 
  , "ugly_col   : Color = #black"
  , "nice_col   : Color = #rainbow"
  ]

{-
testmain = do
  let eilist = parseSrc "example.src" exampleSrc
  let decls = rights eilist
  let CheckState messages info = execState (checkDeclList emptyScope decls) emptyCheckState
  mapM_ print (reverse messages)
  mapM_ print (Map.toList info)
-}

--------------------------------------------------------------------------------
-- * misc

nop :: Monad m => m ()
nop = return ()

--------------------------------------------------------------------------------
