default: build

# Comment out this line to build with `stack`
CABAL=1

build:
ifdef CABAL
	cabal new-build
else
	stack build
endif

analysis: build
ifdef CABAL
	cabal new-run -- hs-pattrans --print -CFEAR
else
	stack exec -- hs-pattrans --print -CFEAR
endif

compare: build
ifdef CABAL
	cabal new-run -- hs-pattrans --print -MCF
else
	stack exec -- hs-pattrans --print -MCF
endif

query:
ifdef CABAL
	cabal new-repl
else 
	stack repl
endif

test: build
ifdef CABAL
	cabal new-test
else
	stack test
endif

docs: build
ifdef CABAL
	cabal new-haddock --haddock-for-hackage --haddock-hyperlink-source
else
	stack exec -- haddock --html src/*.hs --hyperlinked-source
endif

deploy: build docs
ifdef CABAL
	cabal new-run -- hs-pattrans -CE && ./prepare.sh
else
	stack exec -- hs-pattrans -MCFEAR && ./prepare.sh
endif

clean:
ifdef CABAL
	cabal new-clean
else
	stack clean
endif
	rm -rf docs/out && rm -f docs/index.html

.PHONY: default build analysis compare query test docs deploy clean
