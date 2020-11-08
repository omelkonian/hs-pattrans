default: build

# To use Stack, e.g. call `$ make STACK=1 build` or just uncomment the line below:
#STACK=1

build:
ifndef STACK
	cabal new-build --enable-shared
else
	stack build
endif

run: build
ifndef STACK
	cabal new-run -- hs-pattrans $(RUN_ARGS)
else
	stack exec -- hs-pattrans $(RUN_ARGS)
endif

help: 
	$(MAKE) RUN_ARGS="-h" run

query: build
ifndef STACK
	cabal new-repl
else 
	stack repl
endif

test: build
ifndef STACK
	cabal new-test
else
	stack test
endif

docs: build
ifndef STACK
	cabal new-haddock --haddock-for-hackage --haddock-hyperlink-source
else
	stack exec -- haddock --html src/*.hs --hyperlinked-source
endif

website: build docs
	$(MAKE) RUN_ARGS="--html -F d:folk+d:classical+d:heman -A approx6 -ELR" run

analysis: build
	$(MAKE) RUN_ARGS="--print --filters d:classical -EL" run

compare: build
	$(MAKE) RUN_ARGS="--print --filters d:folk -M" run

clean:
ifndef STACK
	cabal new-clean
else
	stack clean
endif
	rm -rf out

.PHONY: default build run help query test docs analysis compare website clean
