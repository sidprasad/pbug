{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.Two where
import PBug.Quote
import PBug.CodeGen

-- Left recursion

[pbug| E=>E"a"< > E=>"b"< > |]

first = parseE "aaaab"


