default: analysis

query:
	stack repl

build:
	stack build

analysis: build
	stack exec -- hs-mirex -PXRCFEA && ./prepare.sh

compare: build
	stack exec -- hs-mirex -FM

docs: build
	stack exec -- haddock --html \
		src/Main.hs src/Types.hs src/Transformations.hs src/Parser.hs \
		src/Analysis.hs src/Charts.hs src/MIDI.hs \
		--hyperlinked-source --odir=docs/haddock

clean:
	stack clean && rm -rf docs/out && rm -rf docs/haddock && rm docs/charts.html

.PHONY: default all docs clean
