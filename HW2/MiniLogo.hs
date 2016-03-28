-- Benjamin Arvey (arveyb)
-- Brandon Lee (leebran)
-- Godfrey Yeung (yeungg)

module MiniLogo where

import Prelude hiding (Num)
import Data.List

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down
          | Up
    deriving (Eq, Show)

data Expr = VarEx Var
          | NumEx Num
          | Add Expr Expr
    deriving (Eq, Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
    deriving (Eq, Show)

--
-- line in concrete syntax:
-- define line (x1, y1, x2, y2) {
--     pen up;
--     move (x1, y1);
--     pen down;
--     move (x2, y2);
-- }
--

line :: Cmd
line = Define "line"
              ["x1", "y1", "x2", "y2"]
              [Pen Up,
               Move (VarEx "x1") (VarEx "y1"),
               Pen Down,
               Move (VarEx "x2") (VarEx "y2"),
               Pen Up]

--
-- nix in concrete syntax:
-- define nix (x, y, w, h) {
--     call line (x, y, x + w, y + h);
--     call line (x + w, y, x, y + h);
-- }
--

nix :: Cmd
nix = Define "nix"
             ["x", "y", "w", "h"]
             [Call "line" [VarEx "x",
                           VarEx "y",
                           Add (VarEx "x") (VarEx "w"),
                           Add (VarEx "y") (VarEx "h")],
              Call "line" [Add (VarEx "x") (VarEx "w"),
                           VarEx "y",
                           VarEx "x",
                           Add (VarEx "y") (VarEx "h")]]

steps :: Int -> Prog
steps n = Pen Down : stepsHelper n

stepsHelper :: Int -> Prog
stepsHelper 0 = []
stepsHelper n = stepsHelper (n - 1) ++ [Move (NumEx (n - 1)) (NumEx (n - 1)),
                                        Move (NumEx (n - 1)) (NumEx n),
                                        Move (NumEx n) (NumEx n)]

macros :: Prog -> [Macro]
macros p = [getDefineName c | c <- p, isDefine c]

isDefine :: Cmd -> Bool
isDefine (Define _ _ _) = True
isDefine _              = False

getDefineName :: Cmd -> Macro
getDefineName (Define name _ _) = name
getDefineName _                 = "Error"


pretty :: Prog -> String
pretty p = concat (map prettyCmd p)

prettyCmd :: Cmd -> String
prettyCmd (Pen m)                  = "pen " ++ (prettyMode m) ++ ";\n"
prettyCmd (Move x y)               = "move (" ++ (prettyExpr x) ++ ", " ++ (prettyExpr y) ++ ");\n"
prettyCmd (Define macro args cmds) = "define " ++ macro ++ " " ++ (prettyArgs args) ++ " {\n" ++ (pretty cmds) ++ "};\n"
prettyCmd (Call macro args)        = "call " ++ macro ++ " " ++ (prettyExprs args) ++ ";\n"

prettyMode :: Mode -> String
prettyMode Up   = "up"
prettyMode Down = "down"

prettyExprs :: [Expr] -> String
prettyExprs exprs = "(" ++ concat (intersperse ", " (map prettyExpr exprs) ) ++ ")"

prettyExpr :: Expr -> String
prettyExpr (VarEx v) = v
prettyExpr (NumEx n) = show n
prettyExpr (Add a b) = (prettyExpr a) ++ " + " ++ (prettyExpr b)

prettyArgs :: [Var] -> String
prettyArgs vs = "(" ++ (concat (intersperse ", " vs)) ++ ")"

optE :: Expr -> Expr
optE (Add (NumEx a) (NumEx b)) = NumEx (a + b)
optE (Add a b)                 = case (optE a, optE b) of
                                   ((NumEx aa), (NumEx bb)) -> NumEx (aa + bb)
                                   (aa, bb)                 -> Add aa bb
optE x                         = x

optP :: Prog -> Prog
optP p = map pHelper p

pHelper :: Cmd -> Cmd
pHelper (Move x y)     = Move (optE x) (optE y)
pHelper (Call m exprs) = Call m [optE expr | expr <- exprs]
pHelper c              = c
