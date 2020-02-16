# Build instructions [![Build Status](https://travis-ci.com/omelkonian/hs-pattrans.svg?branch=master)](https://travis-ci.com/omelkonian/hs-pattrans) [![Hackage](https://img.shields.io/hackage/v/hs-pattrans.svg)](http://hackage.haskell.org/package/hs-pattrans)

- To run the analysis: `make analysis`

- To run an interactive shell, where you can discover patterns using
transformation queries:
  1. `make query`

  2. - Using time period from song: `"bach" ?? (transpositionOf ~~ 0.6) :@ (21,28)`
     - or with Euterpea: `"bach" ?? (transpositionOf ~~ 0.6) :@ (line $ map ($qn) [c 4, e 4, g 4, c 5])`

  3. view extracted MIDI at `data/extracted/bach/`

- To run a cross-dataset comparison (between expert and algorithmic prototypes): `make compare`

- To update online resources:
  1. `make deploy`
  2. Commit and push changes
  3. View generated graphs on the repo's [Github page](https://omelkonian.github.io/hs-pattrans/)
  4. If version changes, view updated package on [Hackage](http://hackage.haskell.org/package/hs-pattrans)
