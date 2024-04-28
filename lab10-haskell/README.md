# lab10-haskell

## Setup

stack install ...
cabal install --lib ...
## Usage

To compile the program:
ghc --make -threaded app/Main.hs -o main
ghc --make -threaded -package async app/Main.hs -o main


To run:
./main.exe