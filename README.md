# Build instructions

- To build: `stack build`


- To run the analysis: `stack exec hs-mirex`
  * Pie-charts will be generated in `output/`


- To generate the docs: `stack exec -- haddock --html src/Main.hs src/Types.hs src/Parser.hs src/Analysis.hs src/Charts.hs --hyperlinked-source --odir=dist/docs`
  * You can then open `dist/docs/index.html` with your web browser
