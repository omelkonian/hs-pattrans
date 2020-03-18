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

website:
	$(MAKE) RUN_ARGS="--html" run

analysis: build
	$(MAKE) RUN_ARGS="--print --filters d:classical -EL" run

compare: build
	$(MAKE) RUN_ARGS="--print --filters d:folk -M" run

deploy: build docs
	$(MAKE) RUN_ARGS="-F d:folk+d:classical+d:heman -A approx6 -ELR" run

clean:
ifdef CABAL
	cabal new-clean
else
	stack clean
endif
	rm -rf out

.PHONY: default build run help query test docs analysis compare deploy clean
