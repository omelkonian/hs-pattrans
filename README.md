# Build instructions [![Build Status](https://github.com/omelkonian/hs-pattrans/workflows/CI/badge.svg)](https://github.com/omelkonian/hs-pattrans/actions) [![Hackage](https://repology.org/badge/version-for-repo/hackage/haskell:hs-pattrans.svg)](http://hackage.haskell.org/package/hs-pattrans)

- To build the source code: `make build`

- To run the testsuite: `make test`

- To check command options: `make help`

- To run an interactive shell, where you can discover patterns using transformation queries:
  1. `make query`

  2. - Using time period from song: `("classical", "bach") ?? (transpositionOf ~~ 0.6) :@ (21,28)`
     - or with Euterpea: `("classical", "bach") ?? (transpositionOf ~~ 0.6) :@ (line $ map ($qn) [c 4, e 4, g 4, c 5])`

  3. view extracted MIDI at `data/extracted/bach/`

- To update online resources:
  1. `make deploy`
  2. Commit and push changes
  3. View generated graphs on the repo's [Github page](https://omelkonian.github.io/hs-pattrans/)
  4. If version changes, view updated package on [Hackage](http://hackage.haskell.org/package/hs-pattrans)

# Troubleshooting

We only currently support/test the following configurations:
- **Linux:** `GHC-8.0.2` with `cabal`
- **MacOS:** `LTS-10.09 (GHC-8.2.2)` with `stack`

You can always refer to the Github action [workflow](https://github.com/omelkonian/hs-pattrans/blob/master/.github/workflows/ci.yml) for the exact commands we run in a fresh VM.

## Linux

- You must install the `asound` package:
```bash
sudo apt-get install libasound2-dev
```

## MacOS

- In case building fails with an error referring to `libiconv`, you should disable it in `port/homebrew`.
