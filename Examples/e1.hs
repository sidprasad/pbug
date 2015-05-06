{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.One where
import PBug.Quote
import PBug.CodeGen
import PBug.Syntax

{-- Parser for the language
 - E => aE
 -    | b
 - --}



[pbug| E=>"a" E< > E=>"b"< > |]

works = parseE "aaaab"
err = parseE "aa"

