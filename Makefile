default: build

query:
	stack repl

build:
	stack build

analysis: build
	stack exec -- hs-pattrans --print -CFEAR

compare: build
	stack exec -- hs-pattrans --print -MCF

test: build
	stack test

docs: build
	stack exec -- haddock --html src/*.hs --hyperlinked-source --odir=docs/haddock

deploy: build docs
	stack exec -- hs-pattrans -MCFEAR && ./prepare.sh

clean:
	stack clean && rm -rf docs/out && rm -rf docs/haddock && rm docs/charts.html

.PHONY: default all docs clean
