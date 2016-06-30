module Lambda where

import Text.Parsec (parse, ParseError, many, many1, (<|>), sepBy1)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Data.Maybe (fromMaybe)

type Id = String
type Def = (Id, Term)

showDef :: Def -> String
showDef (v, t) = concat [v, " = ", showTerm t]

readDef :: String -> Either ParseError Def
readDef = parse parseDef ""

readDefs = parse (sepBy1 parseDef (symbol ";")) ""

parseDef :: Parser Def
parseDef = do v <- identifier
              symbol "="
              t <- parseTerm
              return (v, t)

data Term
  = Lam Id Term
  | App Term Term
  | Var Id
  deriving (Show, Read, Eq)

run :: String -> Either ParseError String
run s = do defs <- readDefs s
           pure $ showTerm . eval defs $ Var "main"

type Env = [Def]

eval :: Env -> Term -> Term
eval e (App (Lam v t) y) = eval ((v, y):e) t
eval e t = case t of
  App x y -> reEval $ App (eval e x) (eval e y)
  Var v   -> reEval $ fromMaybe (Var v) $ lookup v e
  Lam v t -> Lam v $ eval ((v, Var v):e) t
  where reEval new = if t == new then new else eval e new

showTerm :: Term -> String
showTerm (Lam v t) = concat ["(λ", v, ".", showTerm t, ")"]
showTerm (App x y) = concat ["(", showTerm x, " ", showTerm y, ")"]
showTerm (Var v)   = v

readTerm :: String -> Either ParseError Term
readTerm = parse parseTerm ""

parseTerm :: Parser Term
parseTerm = do x <- nonApp
               (foldl App x <$> many nonApp) <|> pure x
  where nonApp = parens parseTerm
             <|> do lambda
                    vs <- many1 identifier
                    symbol "."
                    t <- parseTerm
                    return $ foldr Lam t vs
             <|> Var <$> identifier
        lambda = symbol "\\" <|> symbol "λ"

lexer      = P.makeTokenParser haskellDef
parens     = P.parens     lexer
identifier = P.identifier lexer
symbol     = P.symbol     lexer
