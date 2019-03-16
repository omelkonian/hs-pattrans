default: analysis

query:
	stack repl

build:
	stack build

analysis: build
	stack exec -- hs-mirex -RCFEA && ./prepare.sh

compare: build
	stack exec -- hs-mirex -MCF

docs: build
	stack exec -- haddock --html src/*.hs --hyperlinked-source --odir=docs/haddock

clean:
	stack clean && rm -rf docs/out && rm -rf docs/haddock && rm docs/charts.html

.PHONY: default all docs clean
