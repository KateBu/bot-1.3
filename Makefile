.PHONY: fmt build run tests 

fmt:
	cd src && ormolu --mode inplace $$(find . -name '*.hs')

fmtTests:
	cd test && ormolu --mode inplace $$(find . -name '*.hs')

build:
	stack build --ghc-options="-Wall -Werror"

run: 
	stack run 

tests:
	stack test 
	