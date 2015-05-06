module PBug.PrettyPrint where
import PBug.Syntax
import Data.Set as DS
import Data.List as List


ppGrm :: Grammar -> String
ppGrm (PBG rules) =
            List.foldl (\ acc y -> acc ++ (ppRule y)) "" rules

ppRule :: Rule -> String
ppRule (BNFRule rn (Exprof e) hc) = let

                getStr [] = ""
                getStr ((Lit l):xs) = "\"" ++ l++ "\""  ++" "++(getStr xs)
                getStr ((Rname l):xs) = l++" "++(getStr xs)

            in
                rn ++ " => " ++ (getStr e) ++ "<" ++ hc ++ "> "

ppSet :: Set Term -> String
ppSet s = 
        let
          xs = DS.elems s
          
          getElems acc (Lit l) = (l++ " "++acc)
          getElems acc (Rname r) = (r++ " " ++ acc)
        in
            " {" ++ (List.foldl getElems "}" xs)
