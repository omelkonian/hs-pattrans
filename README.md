# Build instructions

- To build: `stack build`


- To run the analysis: `stack exec hs-mirex`


- To update the docs: `stack exec -- haddock --html src/Main.hs src/Types.hs src/Transformations.hs src/Parser.hs src/Analysis.hs src/Charts.hs --hyperlinked-source --odir=docs/haddock`


- To view the charts/docs online, go [here](https://omelkonian.github.io/hs-mirex/)
