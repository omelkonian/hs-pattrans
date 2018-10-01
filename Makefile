default: analysis

build:
	stack build

analysis: build
	stack exec hs-mirex && ./prepare.sh

docs: build
	stack exec -- haddock --html src/Main.hs src/Types.hs src/Transformations.hs src/Parser.hs src/Analysis.hs src/Charts.hs --hyperlinked-source --odir=docs/haddock

clean:
	stack clean

.PHONY: default all docs clean
