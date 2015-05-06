
module PBug.Semant where
import System.IO
import PBug.PrettyPrint as Pretty
import Text.Parsec hiding (upper,lower)
import PBug.Syntax  -- Defines syntx tree for PADS forms
import PBug.Parser
import Language.Haskell.TH   -- Defines data structures for Haskell Code
import Data.Char (isUpper, isLower)
import Control.Monad (guard)
import Data.Char        -- Provides isDigit and isSpace functions
import Data.List as List
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.Set as DS
import Data.Map as DM
import Text.Printf
import System.Exit
import System.IO.Unsafe

call f (Right x) = f x


checkNonTerminals (PBG rules) = 
                     let
                        lhs = findLHS (PBG rules)
                        nTerms = findNonTerminals (PBG rules)
                        terms = findTerminals (PBG rules)
                        diff = DS.difference nTerms lhs
                    
                       in 
                        (if(DS.isSubsetOf nTerms lhs)
                                    then (False, "")
                                    else  (True,"Error! Unrecognized terms " ++ (Pretty.ppSet diff)
                                            ++ " in productions."))


checkLeftRecursion :: Grammar -> (Bool, String)
checkLeftRecursion (PBG rules) =
  let
        checkLR (c, s) (BNFRule rn e _) = 
            let
                leftRec = case e of
                           (Exprof ((Rname x):terms)) -> (x == rn)
                           _ -> False 
            in
                if c then (c, s) else if leftRec then
                    (True, "Left recursion in rule " ++ rn) else (c,s)
                
  in
     List.foldl checkLR (False, "") rules



findLHS :: Grammar -> Set Term
findLHS (PBG rules) = 
                    let
                        traverseRule acc (BNFRule rn _  _)= (DS.insert (Rname rn) acc)
                    in
                    (List.foldl traverseRule DS.empty rules)


findTerminals :: Grammar -> Set Term
findTerminals (PBG rules) =
                    let
                        traverseTerms acc (Lit l) = (DS.insert (Lit l) acc)
                        traverseTerms acc (Rname r) = acc

                        traverseRule acc (BNFRule _ (Exprof terms) _) =
                                       DS.union (List.foldl traverseTerms DS.empty terms) acc
                    in
                        (List.foldl traverseRule DS.empty rules)


findNonTerminals :: Grammar -> Set Term
findNonTerminals (PBG rules) =
                    let
                        traverseTerms acc (Rname l) = (DS.insert (Rname l) acc)
                        traverseTerms acc (Lit _) = acc

                        traverseRule acc (BNFRule _ (Exprof terms) _) =
                                       DS.union (List.foldl traverseTerms DS.empty terms) acc
                    in
                        (List.foldl traverseRule DS.empty rules)





-- No difference between first and predict!
createFirstTable :: Grammar -> Map Term (Set Term)
createFirstTable (PBG rules) = let

                getFirsts acc (BNFRule rn (Exprof ((Lit l):es))  _) = 
                            case (DM.lookup (Rname rn) acc) of
                             Nothing -> DM.insert (Rname rn) (DS.singleton (Lit l)) acc
                             (Just s) -> DM.insert (Rname rn) (DS.insert (Lit l) s) acc

                -- First-first conflict
                getFirsts acc (BNFRule rn (Exprof ((Rname r):es) ) _) = 
                                    DM.insert (Rname rn) (DS.singleton (Rname r)) acc
                
                getFirsts acc _ = acc
            in
            -- Now we could map across this, and see if there is ever a Rname
            -- in the set for anything. If so, print first-first conflict for it
                List.foldl getFirsts DM.empty rules


-- This one is for semantic checks, reduces everything to literals!
createFollowTable' :: Grammar -> Map Term (Set Term) -> Map Term (Set Term) -> Map Term (Set Term)
createFollowTable' (PBG rules) firsts seconds =
        let
            hd = head rules
            fname = case hd of
                    (BNFRule rn _ _) -> (Rname rn)

            zero = case DM.lookup fname seconds of
                    Nothing -> DM.insert fname (DS.singleton (EOF)) seconds
                    (Just s) -> DM.insert fname (DS.insert (EOF) s) seconds

            expFollow acc (BNFRule rn (Exprof []) _) = acc
            -- Production A -> aB
            expFollow acc (BNFRule rn (Exprof (x:[])) _) =
                          let
                             s = case DM.lookup (Rname rn) acc of
                                    Nothing -> DS.empty
                                    (Just st)  -> st
                          in
                             case (DM.lookup x acc) of
                               Nothing   -> DM.insert x s acc
                               (Just x') -> DM.insert x (DS.union x' s) acc

            -- Production A -> aBb
            expFollow acc (BNFRule rn (Exprof (x:y:xs)) hc) =
                          let
                             s = case DM.lookup y firsts of
                                Nothing -> DS.singleton y -- Nothing means must be a Literal
                                (Just st)  -> st
                          in
                            --This may not work b/c of overwrite
                             DM.union (expFollow acc (BNFRule rn (Exprof (y:xs)) hc)) 
                            (case (DM.lookup x acc) of
                               Nothing   -> DM.insert x s acc
                               (Just x') -> DM.insert x (DS.union x' s) acc) 

            
            s = (List.foldl expFollow zero rules)
            in 
                s

checkFirstFollow g = 
                let
                firsts = createFirstTable g
                follows = createFollowTable' g firsts DM.empty
                
                cff x =  let
                        fs = case DM.lookup x firsts of
                                Nothing -> (DS.singleton x) -- only for literals
                                (Just f1) -> f1

                        ss = case DM.lookup x follows of
                                Nothing -> (DS.empty) 
                                (Just s1) -> s1
                        in       
                            (x,(DS.intersection fs ss))

                conflict (c, err) (Rname x, s) = if (DS.null s) then (c, err) else
                         (True, "First-follow conflict with rule "++x ++ " with intersection"
                                                ++(Pretty.ppSet s))
                -- Have a problem here, parsing is weird,need to rewrite this bit :(
                conflict (c, err) (Lit x, s) = (c, err)
                                            {--if (DS.null s) then (c, err) else
                         (True, "First-follow conflict with non-terminal "++x++ " with intersection"
                                                ++(Pretty.ppSet s))--}



                in
                  (List.foldl conflict (False, "") (DS.elems (DS.map cff (DS.union (findLHS g) (findTerminals g)))))

-- Write another checkFirstFirst that goes through everything and makes sure there is no intersection!

checkFirstFirst' g =
                let
                   firsts = createFirstTable g
                   lhs = DS.elems (findLHS g)
                   

                   fcheckRule (c, err) (Rname r1) = 
                         let
                             checkRule acc (Rname r2) = 
                                 let
                                     (Just s1) = DM.lookup (Rname r1) firsts
                                     (Just s2) = DM.lookup (Rname r2) firsts
                                     s3 = DS.intersection s1 s2
                                 in
                                     if c then (c, err) else
                                     if (r1 == r2) then (False, "") else
                                     if (DS.null s3) then (False, "")
                                     else (True, "First-first conflict between "++r1
                                                           ++ " and "++r2++" with intersection " ++
                                                           (Pretty.ppSet s3))
                        in
                            if c then (c, err)
                            else List.foldl checkRule (c, err) lhs
                in
                   List.foldl fcheckRule (False, "") lhs
                

checkFirstFirst g =
                let
                   firsts = createFirstTable g
                   lhs = DS.elems (findLHS g)
 
                   search [] = (False, "")
                   search ((Lit l):xs) = search xs
                   search ((Rname r):xs) = (True, "First-first conflict between "++r++" and ")

                   traverse (c', err') (Rname r) = 
                                let
                                  (Just s) = DM.lookup (Rname r) firsts
                                  (c, err) = search (DS.elems s)
                                in
                                  if c' then (c', err') else if c then (c, err++r)
                                  else (c', err')
                in
                    List.foldl traverse (False, "") lhs
-------------------------------------------------------------------------------


callCFT(Right x) =
                createFollowTable x (createFirstTable x) DM.empty



---------------------------------------------------------------------------------

createFollowTable :: Grammar -> Map Term (Set Term) -> Map Term (Set Term) -> Map Term (Set Term)
createFollowTable (PBG rules) firsts seconds =
        let

            hd = head rules
            fname = case hd of
                    (BNFRule rn _ _) -> (Rname rn)

            zero = case DM.lookup fname seconds of
                    Nothing -> DM.insert fname (DS.singleton EOF) seconds
                    (Just s) -> DM.insert fname (DS.insert EOF s) seconds
            
            expFollow acc (BNFRule rn (Exprof []) _) = acc
            -- Production A -> aB
            expFollow acc (BNFRule rn (Exprof (x:[])) _) =
                          let
                             s = case DM.lookup (Rname rn) acc of
                                    Nothing -> DS.empty
                                    (Just st)  -> st
                          in
                             case (DM.lookup x acc) of
                               Nothing   -> DM.insert x s acc
                               (Just x') -> DM.insert x (DS.union x' s) acc

            -- Production A -> aBb
            -- This case needs to be examined a little
            expFollow acc (BNFRule rn (Exprof (x:y:xs)) hc) = 
                        let
                             s = DS.singleton y 
                        in
                             DM.union (expFollow acc (BNFRule rn (Exprof (y:xs)) hc)) 
                             (case (DM.lookup x acc) of
                               Nothing   -> DM.insert x s acc
                               (Just x') -> DM.insert x (DS.union x' s) acc) 

            
            s = (List.foldl expFollow zero rules)
          in 
                s


