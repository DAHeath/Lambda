module Lambda where

import Text.Parsec (parse, ParseError, many, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Data.Maybe (fromMaybe)

type Id = String

data Term
  = Lam Id Term
  | App Term Term
  | Var Id
  deriving (Show, Read, Eq)

run :: String -> Either ParseError String
run s = showTerm . eval [] <$> readTerm s

type Env = [(String, Term)]

eval :: Env -> Term -> Term
eval e (App (Lam v t) y) = eval ((v, y):e) t
eval e t = case t of
  App x y -> let new = App (eval e x) (eval e y)
             in if t == new then new else eval e new
  Lam v t -> Lam v $ eval ((v, Var v):e) t
  Var v   -> fromMaybe (Var v) $ lookup v e

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
                    v <- identifier
                    symbol "."
                    t <- parseTerm
                    return $ Lam v t
             <|> Var <$> identifier
        lambda = symbol "\\" <|> symbol "λ"

lexer      = P.makeTokenParser haskellDef
parens     = P.parens     lexer
identifier = P.identifier lexer
symbol     = P.symbol     lexer
