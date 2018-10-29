# Build instructions


- To run the analysis: `make analysis`

- To run an interactive shell, where you can discover patterns using
transformation queries:
  1. `stack repl`
  2. `"bach" ?? (transpositionOf ~~ 0.6) :@ (21,28)`
  3. view extracted MIDI at `data/extracted/bach/`


- To choose from options: `stack build && stack exec -- hs-mirex [-E|--experts] [-A|--algorithms] [-C|--classical] [-F|--folk] [-R|--random] [-X|--export]`


- To update the docs: `make docs`


- To view the charts/docs online:
  1. Commit and push changes
  2. Go [here](https://omelkonian.github.io/hs-mirex/)
