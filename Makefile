.PHONY: fmt build run tests fmtTests hlint

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

hlint :
	cd src && hlint $$(find . -name '*.hs') 
	