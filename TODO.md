TODO list

* look at Data.Sequence (or other options) for listlike [Period] type 
   * Consider performance implications for various types:
     https://github.com/haskell-perf/sequences
   * e.g. Seq are very fast for appending but slower for filter operations
* make Period type use newtype rather than data (see Toying.hs)
