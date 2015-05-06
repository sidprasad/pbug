{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Three where
import PBug.Quote
import PBug.CodeGen

-- First-First Conflict

[pbug|
       E=>B"a"< > 
       B=>"b"< >
|]

first = parseE "aaaab"


