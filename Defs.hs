--
-- CS 430 Spring 2019 P4 (Haskell 2)
--

module Defs where

-- token data type (lexer -> parser)
data Token
  = TInt Int
  | TSym Char
  deriving (Show,Eq)

-- expression data type (built by parser)
data Expr
  = EInt Int
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  deriving (Show,Eq)

-- converts an expression into a shorter, nicer string than does "show"
formatExpr :: Expr -> String
formatExpr expr = case expr of
  EInt i     -> (show i)
  EAdd e1 e2 -> "(" ++ (formatExpr e1) ++ "+" ++ (formatExpr e2) ++ ")"
  ESub e1 e2 -> "(" ++ (formatExpr e1) ++ "-" ++ (formatExpr e2) ++ ")"
  EMul e1 e2 -> "(" ++ (formatExpr e1) ++ "*" ++ (formatExpr e2) ++ ")"

