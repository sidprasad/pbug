{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Four where
import PBug.Quote
import PBug.CodeGen

-- First Follow Conflict

[pbug|
    S=>A "a" "b" < >
    A=> "a" < >
 
|]


first = parseS "aab"


