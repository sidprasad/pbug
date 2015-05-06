{-# LANGUAGE TemplateHaskell  #-}
{-#LANGUAGE NoMonomorphismRestriction #-}

module PBug.CodeGen where
import qualified Text.Parsec.Prim as PP
import PBug.Syntax as PS
import Language.Haskell.ParseMonad
import PBug.Semant as Semant
import Text.Parsec hiding (upper, lower)
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax(Name(..), NameFlavour(..), showName)
import Data.List as List
import Data.Set as DS
import Data.Map as DM
import qualified Data.Maybe as Maybe
import Text.Parsec
import PBug.PrettyPrint
import qualified Text.Parsec.Char as PC
import System.IO.Unsafe
import Data.IORef as DI
import PBug.Parser as PBugParser

--make_pbug_declarations :: Grammar -> Q [Dec]
make_pbug_declarations (g, inp) =  let
                                (PBG rules) = g
                                first = head rules
                                nme = case first of
                                     (BNFRule rn _ _) -> rn
                                argE = (conv' inp)
                                --argE = (conv g)
                                exp = AppE funE argE
                                funE = VarE (mkName("genP"))
                                body = NormalB exp
                            in
                         do
                        {
                         return ([FunD (mkName ("parse"++nme)) [Clause [] body []]])
                        }

conv' s = LitE (StringL s)

{-- This will be fixed in subsequent versions
conv (PBG rules) =
        let

            convRn rn = LitE (StringL rn)
            convHc hc = LitE (StringL hc)

            convT (Lit l) = (AppE (VarE (mkName "Lit")) (LitE (StringL l)))
            convT (Rname r) = (AppE (VarE (mkName "Rname")) (LitE (StringL r)))
            convT EOF = undefined -- Shouldn't happen

            convExpr (Exprof terms) = (AppE (VarE (mkName "Exprof")) (ListE (List.map convT terms)))

            convR (BNFRule rn e hc) = (AppE (AppE (AppE (VarE (mkName "BNFRule")) (convRn rn)) (convExpr e)) (convHc hc))

        in
            (AppE (VarE (mkName "PBug.Syntax.PBG")) (ListE (List.map convR rules)))

--}

genP :: String -> String -> Either ParseError AST
genP s = 
   let
    -- Not proud of this, but for now there is a 2 Parse system:
    -- i. One for error messaging
    -- ii. One to get around Template Haskell scoping rules
    (Right g) = PBugParser.parsePBugGrammar "" 0 0 s
    --
    first = Semant.createFirstTable g
    follow = Semant.createFollowTable g first DM.empty
    lhs = DS.elems (Semant.findLHS g)
  
    --Takes a rulename or Literal an generates a parser of the form:
    --  { parse an element 'a' from first and then an element from follow of 'a' }
    -- should return a Parse AST   --
    -- Strings must be ended with a $ to denote EOF?
    parseFirst EOF = do {
                          x <- (PC.string "$")
                        ; return EndOfFile
                        }
    -- Parse the first, now for that one, parse its follow
    parseFirst (Lit l) = do {
                            x <- (PC.string l)
                           ;y <- parseFollow (Lit l)
                           ; case y of
                            (S z) -> return (T [S x,y])
                            T xs -> return (T ((S x):xs)) 
                            NT rn xs -> return (T [S x, y])
                            EndOfFile -> return (S x)
                        }
    
    parseFirst (Rname r) = 
                    let
                        --fsts is the list of r's firsts
                         fsts = case DM.lookup (Rname r) first of
                                    Nothing -> undefined -- This should never happen
                                    (Just set) -> (DS.elems set)
                         -- All firsts must be literals
                         tryOr acc (Lit l) = (try (parseFirst (Lit l))) <|> acc
                         
                         zero = try (parseFirst (head fsts))
                     in
                        do {
                           x <- List.foldl tryOr zero fsts -- x gets the first and the follow
                        ; return (NT r [x])
                        }

    -- Creates a parser for the follow of something
    -- An element can have no follow
    -- The parser for each possible element of follow should parse its first and follow 
    parseFollow term = let
                        fllws = case DM.lookup term follow of
                                    Nothing -> [] --[EOF]  Set to EOF
                                    (Just set) -> (DS.elems set)
                        tryOr acc t = (try (parseFirst t)) <|> acc
                        zero = if (fllws == []) then (try (parseFirst EOF)) else try (parseFirst (head fllws))
                    in
                        --Now combine all of them
                        List.foldl tryOr zero fllws

    -- Generate a parser for the first expression specified
    (PBG rules) = g
    startS (BNFRule rn _ _) = parseFirst (Rname rn)
    parseGrm = (startS (head rules))


    (unrec, err0) = Semant.checkNonTerminals g
    (leftrec, err1) = Semant.checkLeftRecursion g
    (firstfollow, err2) = Semant.checkFirstFollow g
    (firstfirst, err3) = Semant.checkFirstFirst g
    (firstfirst2, err4) = Semant.checkFirstFirst' g
    eofs = ("$"++eofs)

    -- Also need to write first-first
    in
        \s -> PP.parse ( 
                         if unrec then (error err0) else
                         if leftrec then (error err1) else
                         if firstfirst then (error err3) else
                         if firstfirst2 then (error err4) else
                         if firstfollow then (error err2) else                                            do{
                           
                           x <- parseGrm
                           ;return x
                         }) "" (s++eofs)
         

tryme g str = (genP g) str


