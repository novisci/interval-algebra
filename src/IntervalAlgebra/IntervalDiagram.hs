{-|
Module      : IntervalAlgebra.IntervalDiagram
Description : Tools for visualizing intervals
Copyright   : (c) NoviSci, Inc 2020-2022
                  TargetRWE, 2023
License     : BSD3
Maintainer  : bsaul@novisci.com 2020-2022, bbrown@targetrwe.com 2023

This module provides functions for creating diagrams of intervals as text.
For example,

>>> let ref = bi 30 (0 :: Int)
>>> let ivs = [ bi 2 0, bi 5 10, bi 6 16 ]
>>> pretty $ simpleIntervalDiagram ref ivs
--
          -----
                ------
==============================

Such diagrams are useful for documentation, examples,
and learning to reason with the interval algebra.

There are two main functions available:

* @'parseIntervalDiagram'@:
exposes all available options
and gives the most flexibility in producing diagrams
* @'simpleIntervalDiagram'@
produces simple diagram using defaults.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module IntervalAlgebra.IntervalDiagram
  (
  -- * Make nice-looking diagrams of intervals
  {-|
  All these functions return an @'IntervalDiagram'@,
  which can then be pretty printed using the @'Prettyprinter.pretty'@ function.
  -}
    parseIntervalDiagram
  , simpleIntervalDiagram
  , standardExampleDiagram

  -- * Diagram options
  , IntervalDiagramOptions(..)
  , defaultIntervalDiagramOptions
  , AxisPlacement(..)

  -- * Internal types
  , IntervalText
  , IntervalDiagram

  -- * Errors
  , IntervalTextLineParseError(..)
  , AxisParseError(..)
  , IntervalDiagramOptionsError(..)
  , IntervalDiagramParseError(..)

  -- * Re-exports
  , Prettyprinter.Pretty(..)
  ) where

import           Data.Foldable                     (Foldable (toList))
import qualified Data.List.NonEmpty                as NE hiding (toList)
import           Data.Maybe                        (fromMaybe, isNothing)
import           Data.Text                         (Text, pack)
import           IntervalAlgebra.Core
import           IntervalAlgebra.IntervalUtilities (rangeInterval)
import           IntervalAlgebra.PairedInterval    (PairedInterval, getPairData,
                                                    makePairedInterval)
import           Prettyprinter

-- $setup
-- >>> :set -XTypeApplications -XFlexibleContexts -XOverloadedStrings
-- >>> import IntervalAlgebra.IntervalUtilities
-- >>> import Data.Time

{-
The key Type in this module is the IntervalDiagram,
which has several components.
Each component in sections below organized as follows:
 * Type(s)
 * (optional) Instances
 * (optional) parser
 * (optional) utilities
-}

{-------------------------------------------------------------------------------
  IntervalText
-------------------------------------------------------------------------------}

{-|
@IntervalText@ is an internal type
which contains an @Interval a@ and the @Char@ used to print
the interval in a diagram.

>>> pretty $ makeIntervalText '-' (beginerval 5 (0::Int))
-----
>>> pretty $ makeIntervalText '*' (beginerval 10 (0::Int))
**********
-}

newtype IntervalText a = MkIntervalText (PairedInterval Char a) deriving (Eq, Show)

makeIntervalText :: Char -> Interval a -> IntervalText a
makeIntervalText c = MkIntervalText . makePairedInterval c

instance Intervallic IntervalText where
  getInterval (MkIntervalText x) = getInterval x
  setInterval (MkIntervalText x) i = MkIntervalText $ setInterval x i

instance (Enum (Moment (Interval a)), SizedIv (Interval a)) => Pretty (IntervalText a) where
  pretty (MkIntervalText x) = pretty $ replicate (fromEnum (duration i)) c
   where
    c = getPairData x
    i = getInterval x

{-------------------------------------------------------------------------------
  IntervalTextLine
-------------------------------------------------------------------------------}

{-|
The @IntervalTextLine@ is an internal type
containing a list of @IntervalText@.

Values of this type should only be created
through the 'parseIntervalTextLine' function,
which checks that the inputs are parsed correctly to form intervals
that will be pretty-printed correctly.

>>> let i1 =  makeIntervalText '*' (beginerval 10 (5::Int))
>>> let i2  = makeIntervalText '-' (beginerval 2 (1::Int))
>>> let x = parseIntervalTextLine [] [i1, i2]
>>> pretty x
UnsortedIntervals
>>> let i1 =  makeIntervalText '*' (beginerval 10 (5::Int))
>>> let i2  = makeIntervalText '-' (beginerval 2 (10::Int))
>>> let x = parseIntervalTextLine [] [i1, i2]
>>> pretty x
ConcurringIntervals
>>> let i1 =  makeIntervalText '*' (beginerval 10 ((-1)::Int))
>>> let i2  = makeIntervalText '-' (beginerval 2 (10::Int))
>>> let x = parseIntervalTextLine []  [i1, i2]
>>> pretty x
BeginsLessThanZero
>>> let i1 =  makeIntervalText '*' (beginerval  5 (0::Int))
>>> let i2  = makeIntervalText '-' (beginerval 2 (10::Int))
>>> let x = parseIntervalTextLine [] [i1, i2]
>>> pretty x
*****     --
>>> let i1 =  makeIntervalText '*' (beginerval  5 (5::Int))
>>> let i2  = makeIntervalText '-' (beginerval 2 (10::Int))
>>> let x = parseIntervalTextLine [] [i1, i2]
>>> pretty x
     *****--
>>> let i1 =  makeIntervalText '*' (beginerval  1 (5::Int))
>>> let i2  = makeIntervalText '-' (beginerval 1 (7::Int))
>>> let x = parseIntervalTextLine [] [i1, i2]
>>> pretty x
     * -
>>> let i1 =  makeIntervalText '*' (beginerval  3 (5::Int))
>>> let i2 = makeIntervalText '-' (beginerval 5 (10::Int))
>>> let i3 = makeIntervalText '#' (beginerval 1 17)
>>> pretty $ parseIntervalTextLine [] [i1, i2, i3]
     ***  -----  #
-}
data IntervalTextLine a = MkIntervalTextLine [IntervalText a] [Text]
  deriving Show

{-
NOTE:
a pretty-printed @IntervalTextLine@ does not print its labels.
Line labels are printed by @IntervalDiagram@.
This is because line labels are vertically aligned across lines,
and without the other lines we don't know where to align labels.
-}
instance Pretty (IntervalTextLine Int) where
  pretty (MkIntervalTextLine ivs _) =
    concatWith (<>) (fmap (\x -> indent (begin x) (pretty x)) ivs)

instance Pretty (Either IntervalTextLineParseError (IntervalTextLine Int)) where
  pretty (Left  e) = pretty $ show e
  pretty (Right l) = pretty l

{-|
A type representing errors that may occur
when a list of @IntervalText@ is parsed into a @IntervalTextLine@.
-}
data IntervalTextLineParseError =
    -- | The inputs contains concurring intervals.
    --   All inputs should be @'disjoint'@.
      ConcurringIntervals
    -- | The inputs are not sorted.
    | UnsortedIntervals
    -- | At least one of the inputs has a @'begin'@ less than zero.
    | BeginsLessThanZero
     deriving (Eq, Show, Ord)

{-|
Parses a list of @IntervalText Int@
into an @IntervalTextLine Int@,
handling the types of parse errors that could occur.

See 'IntervalTextLine' for examples.
-}
parseIntervalTextLine
  :: [Text]
  -> [IntervalText Int]
  -> Either IntervalTextLineParseError (IntervalTextLine Int)
parseIntervalTextLine labs l =
  let vals = NE.nonEmpty l
  in  if
        | any (uncurry concur) (pairs l) -> Left ConcurringIntervals
        | (not . isSorted . fmap getInterval) l -> Left UnsortedIntervals
        | any ((< 0) . begin) l -> Left BeginsLessThanZero
        | otherwise -> case vals of
          Nothing -> Right (MkIntervalTextLine [] [])
          Just v ->
            -- The use of makeIntervalLine is important here
            -- in order to get the intervals positioned correctly
            Right $ MkIntervalTextLine (toList (makeIntervalLine v)) labs
 where
  {-
  Modifies the inputs sequentially
  so that the begin of one interval is
  shifted based on the end of the previous interval.
  This function assumes that the inputs are sorted and disjoint.
  -}
  makeIntervalLine
    :: NE.NonEmpty (IntervalText Int) -> NE.NonEmpty (IntervalText Int)
  makeIntervalLine x =
    NE.head x NE.:| zipWith shiftFromEnd (toList x) (NE.tail x)

  -- Creates all pairs of a list
  pairs = go
   where
    go []       = []
    go (x : xs) = fmap (x, ) xs <> go xs
  isSorted xs = and $ zipWith (<=) xs (tail xs)


{-------------------------------------------------------------------------------
  Axis Config and Components
-------------------------------------------------------------------------------}

{-|
A type representing options of where to place the axis in a printed diagram.
-}
data AxisPlacement =
  -- | Print the axis at the top of the diagram
    Top
  -- | Print the axis at the bottom of the diagram
  | Bottom deriving (Eq, Show)

{-|
Key-value list data that can be presented below the axis on an
@IntervalDiagram@. First element of the tuple is an Int key, the second the
Char to print. Note that it does not guarantee uniqueness of the keys, and most
if not all functions should first call @intMapList@ on the internal
@NE.NonEmpty@ list before using this type.
-}
newtype AxisLabels = MkAxisLabels (NE.NonEmpty (Int, Char))
  deriving (Eq, Show)

{-|
A type containing information on
how to configure the axis of an 'IntervalDiagram'.
-}
data AxisConfig = MkAxisConfig
  { placement :: Maybe AxisPlacement
  , labels    :: Maybe AxisLabels
  }
  deriving (Eq, Show)

-- Internal utility to give equivalent structure to IntMap from
-- Data.IntMap.NonEmpty for the key-value list in @AxisLabels@. Previously,
-- when using IntMap for the @AxisLabels@ container, uniqueness and ordering of
-- keys was guaranteed. Now, you should first call this function before using
-- those keys, e.g. in @prettyAxisLabels@, to get the same properties. This has
-- a runtime cost and could be rewritten for efficiency if that were a concern.
-- NOTE: NE does not have a sortOn.
intMapList :: NE.NonEmpty (Int, a) -> NE.NonEmpty (Int, a)
intMapList = NE.sortBy (\(k, _) (k', _) -> compare k k')
  . NE.nubBy (\(k, _) (k', _) -> k == k')

prettyAxisLabels :: AxisPlacement -> AxisLabels -> [Doc ann]
prettyAxisLabels pos (MkAxisLabels labs) = do
  let labssorted = intMapList labs
  let ints       = NE.map fst labssorted
  let marks      = toList $ NE.map snd labssorted
  let labPos =
        NE.head ints : zipWith (\x y -> y - x - 1) (toList ints) (NE.tail ints)
  let out =
        [ hcat $ fmap (\i -> indent i (pretty '|')) labPos
        , hcat $ zipWith indent labPos (pretty <$> marks)
        ]
  case pos of
    Top    -> reverse out
    Bottom -> out

{-------------------------------------------------------------------------------
  Axis
-------------------------------------------------------------------------------}

{-|
A type containing the data necessary to print an axis in an 'IntervalDiagram'.

Use 'parseAxis' for construction.

>>> let ref = makeIntervalText '=' (beginerval 10 (0::Int))


>>> let b = parseAxis [] (Just Top) ref
>>> pretty b
==========

>>> let c = parseAxis [(4, 'a'), (6, 'b')] (Just Top) ref
>>> pretty c
    a b
    | |
==========

>>> let d = parseAxis [(4, 'a'), (6, 'b')] (Just Bottom) ref
>>> pretty d
==========
    | |
    a b

>>> let e = parseAxis [(4, 'a'), (4, 'b')] (Just Top) ref
>>> pretty e
MultipleLabelAtSamePosition

>>> let f = parseAxis [(4, 'a'), (19, 'b')] (Just Top) ref
>>> pretty f
LabelsBeyondReference

-}
data Axis = MkAxis
  { refInterval :: IntervalText Int
  , config      :: AxisConfig
  }
  deriving (Eq, Show)

instance Pretty Axis where
  pretty (MkAxis ref (MkAxisConfig Nothing  _      )) = emptyDoc
  pretty (MkAxis ref (MkAxisConfig (Just _) Nothing)) = pretty ref
  pretty (MkAxis ref (MkAxisConfig (Just Bottom) (Just labels))) =
    vcat $ pretty ref : prettyAxisLabels Bottom labels
  pretty (MkAxis ref (MkAxisConfig (Just Top) (Just labels))) =
    vcat $ prettyAxisLabels Top labels ++ [pretty ref]

instance Pretty ( Either AxisParseError Axis ) where
  pretty (Left  e) = pretty $ show e
  pretty (Right a) = pretty a

{-|
A type representing errors that can occur when parsing an axis.
-}
data AxisParseError =
  -- | Indicates that the position of one ore more axis labels
  --   is outside the reference interval
    LabelsBeyondReference
  -- | Indicates that multiple labels have been put at the same position
  | MultipleLabelAtSamePosition
  deriving (Eq, Show)

{-|
Safely create an @Axis@.

See @Axis@ for examples.
-}
parseAxis
  :: [(Int, Char)]
  -> Maybe AxisPlacement
  -> IntervalText Int
  -> Either AxisParseError Axis
-- if the axis is not shown then any labels are ignored
parseAxis _ Nothing  i = Right $ MkAxis i (MkAxisConfig Nothing Nothing)
parseAxis l (Just p) i = do
  let labels          = intMapList <$> NE.nonEmpty l
  let labPos          = NE.map fst <$> labels
  let inputLabelCount = length l
  if
    |
-- Flag if any of the label positions are beyond the reference interval
      any (\x -> x < begin i || x > end i) (fmap fst l) -> Left
      LabelsBeyondReference
    |
-- Identify if the number of elements in the input list is different
-- from the number of elements after transforming the list
-- into a nonempty IntMap.
-- If different, then flag.
      inputLabelCount > 0 && fmap length labels /= Just inputLabelCount -> Left
      MultipleLabelAtSamePosition
    |
-- Otherwise, we have a good Axis.
      otherwise -> Right
    $  MkAxis i (MkAxisConfig (Just p) (fmap MkAxisLabels labels))

{-------------------------------------------------------------------------------
  IntervalDiagramOptions
-------------------------------------------------------------------------------}

{-|
A record containing options for printing an @'IntervalDiagram'@.
-}
data IntervalDiagramOptions = MkIntervalDiagramOptions
  { -- | See 'PrettyPrinter.LayoutOptions'
    layout      :: LayoutOptions
    -- | Number of spaces to pad the left of the diagram by.
    --   Must be greater than or equal to @0@.
  , leftPadding :: Int
  }
  deriving (Eq, Show)

{-|
A type representing the types of invalid @'IntervalDiagramOptions'@.
-}
data IntervalDiagramOptionsError =
  -- | Indicates that @'PageWidth'@ is @Unbounded@,
  --   which isn't allowed for an IntervalDiagram.
    UnboundedPageWidth
  -- | Indicates that the left padding in the option is < 0.
  | LeftPaddingLessThan0
  deriving (Eq, Show)

{-|
Takes an initial set of options
and checks that the values are valid,
returning an error if not.

Sorry the indirection in that the input type is also in the output type.
Better might be something like
PossibleOptions -> Either Error ValidOptions
But this works and this code is not exposed to the user.
-}
parseDiagramOptions
  :: IntervalDiagramOptions
  -> Either IntervalDiagramOptionsError IntervalDiagramOptions
parseDiagramOptions opts = if
  | leftPadding opts < 0                       -> Left LeftPaddingLessThan0
  | layoutPageWidth (layout opts) == Unbounded -> Left UnboundedPageWidth
  | otherwise                                  -> Right opts
  where isSorted xs = and $ zipWith (<=) xs (tail xs)

-- | Default 'IntervalDiagramOptions' options
defaultIntervalDiagramOptions :: IntervalDiagramOptions
defaultIntervalDiagramOptions = MkIntervalDiagramOptions defaultLayoutOptions 0

{-------------------------------------------------------------------------------
  IntervalDiagram
-------------------------------------------------------------------------------}

{-|
Type containing the data needed to pretty print an interval document.
-}
data IntervalDiagram a = MkIntervalDiagram
  { -- | The reference interval is the interval based on which 'intervalValues'
   --    are transformed.
   --    It is the only interval that retains the original type.
    reference      :: Interval a
  , axis           :: Axis
  , intervalValues :: [IntervalTextLine Int]
  , options        :: IntervalDiagramOptions
  }
  deriving Show

{-|
Type representing errors that may occur
when parsing inputs into an @'IntervalDiagram'@.

Not every possible state of a "bad" diagram is currently captured
by 'parseIntervalDiagram'.
In particular, line labels can be a source of problems.
The labels accept arbitrary @Text@.
Newline characters in a label would, for example, throw things off.
Labels that extend beyond the @'PrettyPrinter.pageWidth'@
will also cause problems.

-}
data IntervalDiagramParseError =
  -- | Indicates that one or more of the input intervals extend beyond the axis.
    IntervalsExtendBeyondAxis
  -- | Indicates that the reference axis is longer than the @'PageWidth'@
  --   given in the @'IntervalDiagramOptions'@.
  | AxisWiderThanAvailable
  -- | Indicates that left padding is >0
  --   and no axis is printed.
  --   This is considered an error because it be impossible
  --   to know the 'begin' values of intervals in a printed @IntervalDiagram@
  --   that has been padded and has no axis.
  | PaddingWithNoAxis
  -- | Indicates that an error occurring when checking the document options.
  | OptionsError IntervalDiagramOptionsError
  -- | Indicates something is wrong with the @Axis@.
  | AxisError AxisParseError
  -- | Indicates that at least one error occurred when parsing the interval lines.
  | IntervalLineError IntervalTextLineParseError
  deriving (Eq, Show)

instance (SizedIv (Interval a)) => Pretty (IntervalDiagram a) where
  pretty (MkIntervalDiagram _ axis ivs opts) = do

    -- Create a list of pretty IntervalLines
    let intervalLines = fmap pretty ivs

    -- Get the length of the reference interval
    -- in order to determine the column position of line labels
    let refDur        = end (refInterval axis)

    -- Position line labels relative to the reference interval
    -- and the end of the last interval in a line.
    -- NOTE:
    -- This is tricky because the intervals
    -- in a parsed IntervalTextLine are referenced relative
    -- to the previous interval in the line,
    -- not to the reference interval.
    -- See use of makeIntervalLine in parseIntervalTextLine.
    -- This why the intervalLineEnd function is used to determine
    -- the end of the intervals in a line.
    let labelIndents  = fmap ((-) refDur . intervalLineEnd) ivs

    -- Create a list of the line label docs
    let labelLines =
          zipWith (\i l -> indent l (prettyLineLabel i)) ivs labelIndents

    -- Zip together each interval line and its labels horizontally,
    -- then stack all the lines.
    let intervalDiagram = vsep $ zipWith (<>) intervalLines labelLines

    -- Add the the axis in the appropriate position.
    let mainDiagram = case (placement . config) axis of
          Nothing     -> intervalDiagram
          Just Top    -> vcat [pretty axis, intervalDiagram]
          Just Bottom -> vcat [intervalDiagram, pretty axis]

    -- Add any left padding.
    indent (leftPadding opts) mainDiagram

   where
    intervalLineEnd :: IntervalTextLine Int -> Int
    intervalLineEnd (MkIntervalTextLine x _) = sum $ fmap end x

    prettyLineLabel :: IntervalTextLine Int -> Doc ann
    prettyLineLabel (MkIntervalTextLine _ t) = if null t
      then emptyDoc
      else space <> pretty ("<-" :: Text) <> space <> pretty t

instance (SizedIv (Interval a)) =>
  Pretty (Either IntervalDiagramParseError (IntervalDiagram a)) where
  pretty (Left  e) = pretty $ show e
  pretty (Right d) = pretty d

{-|
Parse inputs into a pretty printable document.

This function provides the most flexibility in producing interval diagrams.

Here's a basic diagram that shows
how to put more than one interval interval on a line:

>>> let mkIntrvl c d b = makeIntervalText c (bi d (b :: Int))
>>> let x = mkIntrvl  '=' 20 0
>>> let l1 = [ mkIntrvl '-' 1 4 ]
>>> let l2 = [ mkIntrvl '*' 3 5, mkIntrvl '*' 5 10, mkIntrvl 'x' 1 17 ]
>>> let l3 = [ mkIntrvl '#' 2 18]
>>> pretty $ parseIntervalDiagram defaultIntervalDiagramOptions  [] (Just Bottom) x [ (l1, []), (l2, []), (l3, []) ]
    -
     ***  *****  x
                  ##
====================


We can put the axis on the top:

>>> pretty $ parseIntervalDiagram defaultIntervalDiagramOptions [] (Just Top) x [ (l1, []), (l2, []), (l3, []) ]
====================
    -
     ***  *****  x
                  ##


We can annotate the axis:

>>> pretty $ parseIntervalDiagram defaultIntervalDiagramOptions [(5, 'a')] (Just Bottom) x [ (l1, []), (l2, []), (l3, []) ]
    -
     ***  *****  x
                  ##
====================
     |
     a


We can also annotate each line with labels:

>>> pretty $ parseIntervalDiagram defaultIntervalDiagramOptions [] (Just Bottom) x [ (l1, ["line1"]), (l2, ["line2a", "line2b"]), (l3, ["line3"])  ]
    -                <- [line1]
     ***  *****  x   <- [line2a, line2b]
                  ## <- [line3]
====================


The parser tries to check that the data can be printed.
For example, the default @'Prettyprinter.LayoutOptions'@ is 80 characters.
Providing an reference interval wider than 80 characters
results in an error.

>>> let x = mkIntrvl '=' 100 5
>>> let ivs = [ mkIntrvl '-' 1 1 ]
>>> parseIntervalDiagram defaultIntervalDiagramOptions [] Nothing x [ (ivs, []) ]
Left AxisWiderThanAvailable

See 'IntervalDiagramParseError' for all the cases handled.

-}
parseIntervalDiagram
  :: (Ord a, SizedIv (Interval a), Enum a, Num a, Enum (Moment (Interval a)))
  => IntervalDiagramOptions
  -- ^ Document options (see 'IntervalDiagramOptions')
  -> [(Int, Char)]
  -- ^ A list of axis labels
  -> Maybe AxisPlacement
  -- ^ An optional 'AxisPlacement' of the axis
  -> IntervalText a
  -- ^ The reference (axis interval)
  -> [([IntervalText a], [Text])]
  -- ^ Intervals to include in the diagram.
  -- Each item in the list creates a new line in the printed diagram.
  -- Text creates an optional label for the line.
  -> Either IntervalDiagramParseError (IntervalDiagram a)
parseIntervalDiagram opts labels placement ref ivs =
  case parseDiagramOptions opts of
    Left  e -> Left $ OptionsError e
    Right o -> if
      |
-- check that the duration of the reference intervall
-- does not exceed the page width
        checkAvailableChar (layoutPageWidth $ layout o)
      -> Left AxisWiderThanAvailable
      |
-- check none of the interval extend beyond the reference interval
        any (extendsBeyond ref) (concatMap fst ivs)
      -> Left IntervalsExtendBeyondAxis
      |
-- check that padding == 0 and axis is displayed
        leftPadding o > 0 && isNothing placement
      -> Left PaddingWithNoAxis
      | otherwise
      -> let parsedReferencedIntervals = traverse
               (\(i, t) -> parseIntervalTextLine t (rereferenceL ref i))
               ivs
         in  case parsedReferencedIntervals of
               Left e -> Left $ IntervalLineError e
               Right vals ->
                 let parsedAxis =
                       parseAxis labels placement (rereference ref ref)
                 in  case parsedAxis of
                       Left e -> Left $ AxisError e
                       Right axis ->
                         Right $ MkIntervalDiagram (getInterval ref) axis vals o
 where
  extendsBeyond =
    before <|> meets <|> overlaps <|> overlappedBy <|> metBy <|> after
  checkAvailableChar (AvailablePerLine i _) = fromEnum (duration $ getInterval ref) > i
  checkAvailableChar Unbounded              = True
  {-
    Shifts the endpoints of an interval to be referenced from another interval,
    so that the 'begin' of the reference interval acts as the "zero" point.
  -}
  rereference x = fromEnumInterval . shiftFromBegin x
  rereferenceL x = fmap (rereference x)

{-|
Given a reference interval and a list of intervals,
produces an 'IntervalDiagram' with one line per interval,
using the 'defaultIntervalDiagramOptions'.

>>> pretty $ simpleIntervalDiagram (bi 10 (0 :: Int)) (fmap (bi 1) [0..9])
-
 -
  -
   -
    -
     -
      -
       -
        -
         -
==========

>>> let ref = bi 30 (0 :: Int)
>>> let ivs = [ bi 2 0, bi 5 10, bi 6 16 ]
>>> pretty $ simpleIntervalDiagram ref ivs
--
          -----
                ------
==============================
-}
simpleIntervalDiagram
  :: (Ord a, SizedIv (Interval a), Intervallic i, Enum a, Num a, Enum (Moment (Interval a)))
  => i a -- ^ The axis interval
  -> [i a] -- ^ List of intervals to be printed one per line
  -> Either IntervalDiagramParseError (IntervalDiagram a)
simpleIntervalDiagram ref ivs = parseIntervalDiagram
  defaultIntervalDiagramOptions
  []
  (Just Bottom)
  (makeIntervalText '=' (getInterval ref))
  (fmap (\x -> (pure $ makeIntervalText '-' $ getInterval x, [])) ivs)

{- | Given various inputs containing intervals and their label, creates an
interval diagram with labels, along with a reference range that spans all of the
intervals and is extended to include 0 if necesary.

In more detail, an interval diagram is created with one row in the diagram for
each interval and label pair provided as the first input, and followed by a
sequence of additional rows with one row per list element in the second input
and such that each row displays each interval provided in the intervals list and
label pair.

>>> x1 = beginerval 4 1
>>> x2 = beginerval 3 7
>>> x3 = beginerval 2 13
>>> ivs = [x1, x2, x3]
>>> gaps = [beginerval 2 5, beginerval 3 10]
>>> :{
pretty $ standardExampleDiagram (zip ivs ["x1", "x2", "x3"]) [(gaps, "gaps")]
:}
 ----           <- [x1]
       ---      <- [x2]
             -- <- [x3]
     --   ---   <- [gaps]
===============

>>> :{
pretty $ standardExampleDiagram (zip ivs ["x1", "x2", "x3"]) []
:}
 ----           <- [x1]
       ---      <- [x2]
             -- <- [x3]
===============

>>> pretty $ standardExampleDiagram [] [(gaps, "gaps")]
     --   --- <- [gaps]
=============

>>> pretty $ standardExampleDiagram [] []
IntervalsExtendBeyondAxis
-}
standardExampleDiagram
  :: (Num a, Enum a, Ord a, Enum (Moment (Interval a)), Ord (Moment (Interval a)), SizedIv (Interval a))
  => [(Interval a, String)]
  -> [([Interval a], String)]
  -> Either IntervalDiagramParseError (IntervalDiagram a)
standardExampleDiagram ivs livs = op ref
 where
  op Nothing     = Left IntervalsExtendBeyondAxis
  op (Just ref') = parseIntervalDiagram defaultIntervalDiagramOptions
                                        []
                                        (Just Bottom)
                                        ref'
                                        combIvs
  range         = rangeInterval $ map fst ivs ++ concatMap fst livs
  anchoredRange = case range of
    Nothing  -> Nothing
    (Just x) -> Just $ safeInterval (min (begin x) 0, max (end x) 0)
  ref = fmap (makeIntervalText '=') anchoredRange
  f (iv, s) = ([iv], s)
  g (ivs, s) = (map (makeIntervalText '-') ivs, [pack s])
  combIvs = map (g . f) ivs ++ map g livs
