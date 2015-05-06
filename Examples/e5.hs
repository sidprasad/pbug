{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Fail where
import PBug.Quote
import PBug.CodeGen

-- PBUG ERROR

[pbug|
     E=>"a"E< >
     E=>"b" "a"< > |]

-- This should not parse, but it is
first = parseE "aaaa"


