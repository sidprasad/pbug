
module PBug.Syntax where

import Language.Haskell.TH (Pat, Exp, Strict)
import Data.Generics (Data, Typeable)
import Language.Haskell.TH

data Grammar = PBG [Rule]
  deriving (Eq, Show)

data Rule = BNFRule Rulename Expr HaskellCode
   deriving (Eq, Show)


data Expr = Exprof [Term]
  deriving (Eq, Show) 

data Term = Lit Literal
           |Rname Rulename
           |EOF
    deriving (Eq,Ord, Show)

type Literal = String --literals

type Rulename = String  -- BNF rule names

type HaskellCode = String


data AST = NT String [AST]
        | T [AST]
        |  S String
        | EndOfFile
  deriving (Eq, Show)

