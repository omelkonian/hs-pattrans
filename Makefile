default: build

# Comment out this line to build with `stack`
CABAL=1

build:
ifdef CABAL
	cabal new-build
else
	stack build
endif

run: build
ifdef CABAL
	cabal new-run -- hs-pattrans $(RUN_ARGS)
else
	stack exec -- hs-pattrans $(RUN_ARGS)
endif

help: 
	$(MAKE) RUN_ARGS="-h" run

query: build
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

analysis: build
	$(MAKE) RUN_ARGS="--print --filters d:classical -EL" run

compare: build
	$(MAKE) RUN_ARGS="--print --filters d:folk -CM" run

deploy: build docs
	$(MAKE) RUN_ARGS="-ELRM --print" run && ./prepare.sh

clean:
ifdef CABAL
	cabal new-clean
else
	stack clean
endif
	rm -rf docs/out && rm -f docs/index.html

.PHONY: default build run help query test docs analysis compare deploy clean
