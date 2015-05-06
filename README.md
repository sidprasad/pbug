# PBug
## Siddhartha Prasad

PBug is a parser generator language which auto generates a parser-debugger.
It is currently designed to work with LL(1) and SLR parsers.
PBug is embedded in Haskell.

##To Do

- Figure out how to get `g` in scope in `CodeGen.hs`

## Current Notes

- `$` is a reserved character.
- Certain combinations where the follow set of a non-terminal include the follow
set cause issues, where the often the language++ is decided.

