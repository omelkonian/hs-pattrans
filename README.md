# Build instructions


- To run the analysis: `make analysis`


- To run an interactive shell, where you can discover patterns using
transformation queries:
  1. `make query`

  2. - Using time period from song: `"bach" ?? (transpositionOf ~~ 0.6) :@ (21,28)`
     - or with Euterpea: `"bach" ?? (transpositionOf ~~ 0.6) :@ (line $ map ($qn) [c 4, e 4, g 4, c 5])`

  3. view extracted MIDI at `data/extracted/bach/`


- To run a cross-dataset comparison (between expert and algorithmic prototypes): `make compare`


- To choose from options: `stack build && stack exec -- hs-pattrans [-E|--experts] [-A|--algorithms] [-C|--classical] [-F|--folk] [-R|--random] [-X|--export]`


- To update the docs: `make docs`


- To view the charts/docs online:
  1. Commit and push changes
  2. Go [here](https://omelkonian.github.io/hs-pattrans/)
