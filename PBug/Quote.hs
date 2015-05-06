module PBug.Quote
    (pbug)
    where

import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

--import PBug.CodeGen (filename here)
import qualified PBug.Parser as P
import qualified PBug.CodeGen
import qualified PBug.Syntax
import qualified PBug.Semant


{-
 - Put semantic checks in a monad, run in a do
 - Build first and follow sets after
 -}


pbug :: QuasiQuoter
pbug  = QuasiQuoter {
                    quoteDec = pparse,
                    quoteExp = (error "No expression quasiquoting"),
                    quotePat = (error "No pattern quasiquoting"),
                    quoteType = (error "No type quasiquoting")
                }

pparse input = do
    loc <- TH.location
    let fileName = TH.loc_filename loc
    let (line,column) = TH.loc_start loc
    -- Will stitch in semantic checks somewhere here
    case P.parsePBugGrammar fileName line column input of
      Left err -> unsafePerformIO $ fail $ show err
      Right g  -> PBug.CodeGen.make_pbug_declarations (g, input)



