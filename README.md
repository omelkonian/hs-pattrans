# Build instructions

- To build: `stack build`


- To run the analysis: `stack exec hs-mirex`
  * Pie-charts will be generated in `output/`


- To update the docs: `stack exec -- haddock --html src/Main.hs src/Types.hs src/Parser.hs src/Analysis.hs src/Charts.hs --hyperlinked-source --odir=docs`
  * After commiting the docs, you can view them [online](https://omelkonian.github.io/hs-mirex/)
