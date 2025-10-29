{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Void
import Pretty as P
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

-------------------------------------------------------------------------------
-- AST

newtype Table a = Table [[a]]
  deriving (Eq, Functor, Foldable, Traversable)

tableWidth :: Table a -> Int
tableWidth (Table []) = 0
tableWidth (Table [[]]) = 0
tableWidth (Table (row:_)) = length row

data Scalar
  = SInt Int
  deriving (Eq, Show)

data Value
  = FiniteTable (Table Scalar)
  | Closure
    { clo_nm :: String
    -- ^ The name that should be bound in the environment
    -- when the closure is applied.
    , clo_env :: Map String Value
    -- ^ The environment of values captured by the closure.
    , clo_e :: Term
    -- ^ The body of the closure.
    }

data Term
  = TmVar String
  | TmAbs String Term
  | TmApp Term Term
  | TmLet String Term Term
  | TmTable (Table Term)
  | TmInt Int
  | TmOpApp String Term Term
  | TmNegate Term

-- | Determines the precedence of an operator entirely by its first
-- letter.
opPrec :: String -> Int
opPrec (c:_)
  | c `elem` ( "^" :: String) = 5
  | c `elem` ("*/" :: String) = 4
  | c `elem` ("+-" :: String) = 3
opPrec _ = 11

-------------------------------------------------------------------------------
-- Pretty Printer

instance SDoc Scalar where
  showDoc (SInt i) = showDoc (show i)

instance SDoc a => SDoc (Table a) where
  showDoc (Table []) = text "∅"
  showDoc (Table [[]]) = text "()"
  showDoc (Table rows) =
    P.group ( text "[ "
              <> P.group (mconcat (intersperse (line <> text "; ") (map showRow rows)))
              <> line <> text "]" )
    where
      showRow row =
        mconcat $ intersperse (text ", ") (map showDoc row)

instance SDoc Value where
  showDoc (FiniteTable table) = showDoc table
  showDoc Closure { clo_e } = showDoc clo_e

class Prec a where
  -- | Get the precedence of a term
  prec :: a -> Int

instance Prec Term where
  prec TmApp {} = 9
  prec (TmOpApp op _ _) = opPrec op
  prec TmAbs {} = 0
  prec _ = 11

-- | Show `a`, but surrounded with parenthesis.
showWithParens :: SDoc a => a -> Doc
showWithParens a = text "(" <> showDoc a <> text ")"

-- | Show `a` surrounded with parenthesis, but only if the first
-- argument is @True@.
maybeShowWithParens :: SDoc a => Bool -> a -> Doc
maybeShowWithParens True = showWithParens
maybeShowWithParens False = showDoc

-- | Given two documents, produce a new document describing the
-- application of document `a` to document `b`, where the two
-- documents are allowed to appear on separate lines if necessary.
fmtApp2 :: Doc -> Doc -> Doc
fmtApp2 a b = P.group $ a <> nest 2 (line <> b)

-- | Produce a document describing the application of two terms, where
-- the two terms are allowed to appear on separate lines if necessary.
showApp2 :: (Prec a, SDoc a, Prec b, SDoc b) => Int -> a -> b -> Doc
showApp2 p a b =
  fmtApp2
  (maybeShowWithParens lhsNeedsParens a)
  (maybeShowWithParens rhsNeedsParens b)
  where
    -- Left associative
    lhsNeedsParens = prec a <  p
    rhsNeedsParens = prec b <= p

-- | See `showInfixr`. This function is the same, except that the
-- infix operator is presumed to be /left/ associative.
showInfixl :: (Prec a, SDoc a, Prec b, SDoc b) => Int -> a -> String -> b -> Doc
showInfixl p a sep b = P.group $
  maybeShowWithParens lhsNeedsParens a
  <> text " "
  <> showDoc sep
  <> P.line
  <> maybeShowWithParens rhsNeedsParens b
  where
    -- Left associative
    lhsNeedsParens = prec a <  p
    rhsNeedsParens = prec b <= p

instance SDoc Term where
  showDoc (TmVar x) = showDoc x
  showDoc (TmAbs x e) =
    P.group ( text "\\" <> showDoc x <> text " ->" <> line <> nest 4 (showDoc e) )
  showDoc tm@(TmApp m n) = showApp2 (prec tm) m n
  showDoc (TmLet x e r) =
    P.group ( text "let " <> showDoc x <> text " ="
              <> P.group (line
                          <> nest 4 (showDoc e)
                          <> line <> text "in")
              <> line <> nest 4 (showDoc r) )
  showDoc (TmTable tbl) = showDoc tbl
  showDoc tm@(TmOpApp op lhs rhs) = showInfixl (prec tm) lhs op rhs
  showDoc (TmInt i) = showDoc (show i)
  showDoc (TmNegate t) = text "-" <> showDoc t

-------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void String

skipBlockComment :: Parser ()
skipBlockComment =
  try (string "{-" >> notFollowedBy digitChar)
  >> void (manyTill anySingle (string "-}"))

-- | Consume whitespace and comments
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  skipBlockComment

-- | Transform a parser into a new one which behaves identically,
-- except it also consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Consume a string of `Text`, followed by optional whitespace.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Consume a character which is allowed to make up part of a name.
nameChar :: Parser Char
nameChar = alphaNumChar <|> symbolChar <|> oneOf ['-','_']

-- | A keyword is like a symbol, except it must be immediately
-- followed by a non-name character.
keyword :: String -> Parser String
keyword s = lexeme $ string s <* notFollowedBy nameChar

-- | Transform a parser into one which parses the same thing, but
-- surrounded with parenthesis.
parenthesis :: Parser a -> Parser a
parenthesis p = symbol "(" *> p <* symbol ")"

-- | Parse a (term) name.
pName :: Parser String
pName = lexeme
  -- Check that we're not about to parse a keyword as a name.
  $ notFollowedBy (keyword "in" <|> keyword "let")
  -- Parse a name.
  >> ((:) <$> lowerChar <*> many nameChar)

pInteger :: Parser Int
pInteger = lexeme L.decimal

pRow :: Parser [Term]
pRow = sepBy pTerm (symbol ",")

pTable :: Parser (Table Term)
pTable = symbol "[" *> (Table <$> sepBy pRow (symbol ";")) <* symbol "]"

pAbs :: Parser Term
pAbs = do
  void $ symbol "\\"
  nm <- pName
  void $ symbol "->"
  TmAbs nm <$> pTerm

pLet :: Parser Term
pLet = do
  void $ keyword "let"
  nm <- pName
  void $ symbol "="
  e <- pTerm
  void $ keyword "in"
  TmLet nm e <$> pTerm

pAtom :: Parser Term
pAtom = msum
  [ parenthesis pTerm
  , pAbs
  , pLet
  , TmInt   <$> pInteger
  , TmVar   <$> pName
  , TmTable <$> pTable
  ]

pAppChain :: Parser Term
pAppChain = foldl1 TmApp <$> ((:|) <$> pAtom <*> many pAtom)

-- | Test if the given char can be in an operator.
opChar :: Char -> Bool
opChar c = c `elem` ("+-*/^" :: String)

pTerm :: Parser Term
pTerm = makeExprParser pAppChain table
  where
    table =
      [ [ prefix "-" TmNegate ]
      -- Each @binary@ represents a class of operators, those which
      -- start with the corresponding character (and thus share the
      -- same precedence as other operators with the same prefix).
      , [ binary '^' ]
      , [ binary '*', binary '/' ]
      , [ binary '+', binary '-' ]
      ]

    prefix name f = Prefix  (f <$ symbol name)

    binary pfx = InfixL $ do
      c <- char pfx
      rest <- lexeme $ many (satisfy opChar)
      let op = c:rest
      pure $ TmOpApp op

parseTerm :: MonadError String m => String -> m Term
parseTerm src = case parse (sc *> pTerm <* eof) "" src of
  Left err -> throwError (errorBundlePretty err)
  Right tm -> pure tm

-------------------------------------------------------------------------------
-- Free

class Free a where
  free :: a -> Set String

instance Free Term where
  free (TmVar x) = S.singleton x
  free (TmAbs x m) = S.delete x (free m)
  free (TmApp m n) = free m `S.union` free n
  free (TmLet x m n) = free m `S.union` S.delete x (free n)
  free (TmTable tbl) = foldl' (\a -> S.union a . free) S.empty tbl
  free (TmInt _) = S.empty
  free (TmOpApp _ m n) = free m `S.union` free n
  free (TmNegate m) = free m

-------------------------------------------------------------------------------
-- Evaluation

-- Informally, the semantics of apply are, match the first n columns
-- of the two arguments, returning those leftover columns.

-- let inc = (a -> a + 1) in
-- inc [0; 1; 2] -- [1; 2; 3]

-- let inc = [0, 1; 1, 2; 2, 3]...
-- [0, 1; 1, 2; 2, 3] [0; 1; 2]

-- | Evaluate a term to a value in the given environment.
eval :: MonadError String m => Map String Value -> Term -> m Value
eval env (TmVar x) =
  case M.lookup x env of
    Just v -> pure v
    Nothing -> throwError ("Unbound variable " ++ x)
eval env (TmAbs x m) = pure $ Closure x env' m
  where
    env' = M.filterWithKey (\k _v -> k `S.member` fvs) env
    fvs = free m
eval env (TmApp m n) = do
  mv <- eval env m
  nv <- eval env n
  evalApp mv nv
eval env (TmLet x m n) = do
  mv <- eval env m
  let env' = M.insert x mv env
  eval env' n

-- Empty tables are easy...
eval _env (TmTable (Table [])) = pure $ FiniteTable $ Table []
eval _env (TmTable (Table [[]])) = pure $ FiniteTable $ Table []

-- For nonempty tables, evaluate an equivalent list of applications of
-- @*@ and @+@.
eval env (TmTable (Table (hd:tl))) = eval env =<< xformRows (hd:|tl)
  where
    xformRows :: MonadError String m => NonEmpty [Term] -> m Term
    xformRows rows =
      foldl1 (TmOpApp "+") <$> mapM (fmap xformRow . asNonEmpty) rows

    xformRow :: NonEmpty Term -> Term
    xformRow = foldl1 (TmOpApp "*")

    asNonEmpty :: MonadError String m => [a] -> m (NonEmpty a)
    asNonEmpty [] = throwError "Found an empty row in table literal!"
    asNonEmpty (a:as) = pure $ a :| as
eval _env (TmInt s) = pure $ promoteScalar (SInt s)
eval env (TmOpApp "*" m n) = do
  mv <- eval env m
  nv <- eval env n
  evalTableProduct mv nv
eval env (TmOpApp "+" m n) = do
  mv <- eval env m
  nv <- eval env n
  evalTableSum mv nv
eval _env (TmOpApp op _ _) =
  throwError ("Unknown operator " ++ op)
eval env (TmNegate m) =
  fmap (FiniteTable . negateTable) . asFiniteTable =<< eval env m
  where
    negateTable :: Table Scalar -> Table Scalar
    negateTable = fmap (\(SInt i) -> SInt (-i))

-- | Promote a scalar to a 1x1 finite table.
promoteScalar :: Scalar -> Value
promoteScalar (SInt i) = FiniteTable $ Table [[ SInt i ]]

-- | Evaluate the tabular product (cross product) of the two values.
evalTableProduct :: MonadError String m => Value -> Value -> m Value
evalTableProduct va vb = do
  (Table aRows) <- asFiniteTable va
  (Table bRows) <- asFiniteTable vb

  let rows = [ aRow ++ bRow | aRow <- aRows, bRow <- bRows ]
  pure $ FiniteTable (Table rows)

-- | Evaluate the tabular sum of the first and second values.
evalTableSum :: MonadError String m => Value -> Value -> m Value
evalTableSum va vb = do
  ta@(Table aRows) <- asFiniteTable va
  tb@(Table bRows) <- asFiniteTable vb

  when (tableWidth ta /= tableWidth tb) $
    throwError "Cannot take the sum of tables with unequal widths!"

  pure $ FiniteTable (Table (aRows ++ bRows))

-- | Evaluate the application of the first value to the second. Note
-- that application is symmetric.
evalApp :: MonadError String m => Value -> Value -> m Value
evalApp a@FiniteTable {} b@Closure {} = evalApp b a
evalApp a (FiniteTable (Table bRows)) = do
  chunks <- mapM (go a) bRows
  pure $ FiniteTable $ Table $ concat chunks

  where
    go :: MonadError String m => Value -> [Scalar] -> m [[Scalar]]
    go fun bRow =
      -- For each row of @bRows@, apply @a@ to the first @n@
      -- cells in the row. Once @a@ is no longer a closure, we
      -- can apply two finite tables by filtering on prefix as
      -- usual.
      case fun of
        Closure { clo_nm, clo_env, clo_e } -> do
          (cell, rest) <- case bRow of
                (cell:rest) -> pure (cell, rest)
                -- TODO: We need to do a transformation here in order
                -- to fully apply all partially applied functions.
                [] -> throwError "Function applied to too few arguments"

          -- Apply the function to the first value in @row@,
          -- returning a new value.
          let argv = promoteScalar cell
          let env' = M.insert clo_nm argv clo_env
          v <- eval env' clo_e
          go v rest
        FiniteTable (Table aRows) -> pure $
          -- Compare prefixes of @aRows@ and @bRowRest@.
          flip mapMaybe aRows $ \aRow ->
          let n = min (length aRow) (length bRow)
          in if take n aRow == take n bRow
             then Just $ drop n aRow ++ drop n bRow
             else Nothing

-- | Interpret the given value as a finite table, throwing an error if
-- the value is infinite.
asFiniteTable :: MonadError String m => Value -> m (Table Scalar)
asFiniteTable (FiniteTable t) = pure t
asFiniteTable Closure {} =
  throwError "Found closure where finite table was expected"

-------------------------------------------------------------------------------
-- REPL

repl :: IO ()
repl = do
  putStr "λ> "
  src <- getLine
  res <- runExceptT $ do
    tm <- parseTerm src
    v <- eval M.empty tm
    liftIO $ T.putStrLn (prettyPrint 40 v)
  case res of
    Left err -> putStrLn err
    Right () -> pure ()
  repl

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repl
