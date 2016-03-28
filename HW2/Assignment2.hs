module Minilogo where

import Prelude hiding (Num)

--- Task 1

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down
          | Up
  deriving (Eq,Show)

data Expr = VarExpr Var
          | NumExpr Num
          | Add Expr Expr
  deriving (Eq,Show)

sem :: Expr -> Int
sem (Add l r) = sem l + sem r

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)

--- Task 2

line :: Cmd
line = Define "line"
       ["x1", "y1", "x2", "y2"]
       [
          Pen Up,
          Move (VarExpr "x1") (VarExpr "y1"),
          Pen Down,
          Move (VarExpr "x2") (VarExpr "y2"),
          Pen Up
       ]

--- Task 3

nix :: Cmd
nix = Define "nix"
       ["x", "y", "w", "h"]
       [
          Call "line" [
              VarExpr "x",
              VarExpr "y",
              Add (VarExpr "x") (VarExpr "w"),
              Add (VarExpr "y") (VarExpr "h")
          ],
          Call "line" [
              Add (VarExpr "x") (VarExpr "w"),
              VarExpr "y",
              VarExpr "x",
              Add (VarExpr "y") (VarExpr "h")
          ]
       ]

--- Task 4

steps :: Int -> Prog
steps 0 = []
steps n = Pen Down : steps (n - 1) ++ [Move (NumExpr (n - 1)) (NumExpr (n - 1)),
                                       Move (NumExpr (n - 1)) (NumExpr n),
                                       Move (NumExpr n) (NumExpr n)]

--- Task 5
