import Mirex (decode)

main :: IO ()
main = do
  entries <- decode "JKUPDD-Aug2013/groundTruth/bachBWV889Fg/monophonic/csv/wtc2f20.csv"
  print entries
