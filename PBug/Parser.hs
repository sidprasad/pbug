
module PBug.Parser where
import Text.Parsec hiding (upper,lower)
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim   as PP
import qualified Text.Parsec.Token  as PT
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos      (newPos)


-- Problems with importing and modules here
--import qualified Language.Haskell.Meta as LHM  -- Supports parsing Haskell forms
import PBug.Syntax  -- Defines syntx tree for PADS forms
import Language.Haskell.TH   -- Defines data structures for Haskell Code

import Data.Char (isUpper, isLower)
import Control.Monad (guard)

import Data.Char        -- Provides isDigit and isSpace functions



import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Parser = PS.Parser
type Env = [String]



parsePBugGrammar :: SourceName -> Line -> Column -> String -> Either ParseError Grammar
parsePBugGrammar fileName line column input =
                PP.parse ( do { setPosition (newPos fileName line column)
                                ; whiteSpace
                                ; x <- rules
                                ; whiteSpace
                                ; eof <|> errorParse
                                ; return (PBG x)
                                }) fileName input

errorParse = do
            {rest <- manyTill anyToken eof
            ; unexpected rest }


-- May want to change these to throw away the white space.
aWhite :: Parser Char
aWhite =  char ' ' 
      <|> char '\t' <|> char '\n'

whiteSpace :: Parser [Char]
whiteSpace = many aWhite

rules :: Parser [Rule]
rules = many rule

rule :: Parser Rule
rule = do {
        whiteSpace 
       ; rn <- parseRulename
       ; whiteSpace 
       ; parseArrow
       ; whiteSpace 
       ; e <- parseExpr
       ; whiteSpace
       ; hc <- parseHC
       ;whiteSpace
       ;return (BNFRule rn e hc)
    }

parseRulename :: Parser Rulename
parseRulename = do {
                    name <- many (PC.letter)
                ; return name
                }

parseArrow :: Parser String
parseArrow = do {
                whiteSpace
               ;stalk <- PC.char '='
               ;head <- PC.char '>'
               ;whiteSpace
               ;return [stalk, head]
            }

parseExpr :: Parser Expr
parseExpr = do {
                xs <- many1 parseTerm
               ;return (Exprof xs)
            }

parseTerm :: Parser Term
parseTerm = do {
                whiteSpace
               ; x <- (try parseLit) <|> (parseRHSRulename)
               ;return x
            }

parseLit :: Parser Term
parseLit = do {
            whiteSpace
           ;_ <- PC.satisfy (\x -> x == '"')
           ;x <- many (PC.noneOf ['"','$'])
           ;_ <- PC.satisfy (\x -> x == '"')
           ; return (Lit x)
            }

parseRHSRulename :: Parser Term
parseRHSRulename = do {
                    name <- many1 (PC.letter)
                ; return (Rname name)
                }

-- Adjust this for what Haskell allows
parseHC :: Parser HaskellCode
parseHC = do {
           _ <- PC.char '<'
           ;code <- many (PC.noneOf ">")  -- Maybe many, not many1
           ;_ <- PC.char '>'
           ; return code
            }

test str = parsePBugGrammar "" 0 0 str

