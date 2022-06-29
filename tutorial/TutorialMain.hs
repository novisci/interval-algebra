{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings #-}

module TutorialMain where

-- tag::import-declarations[]
import IntervalAlgebra
import IntervalAlgebra.IntervalDiagram

import Data.Bifunctor ( Bifunctor(..) )
import Data.List ( sort )
import Data.Time
    ( addDays, fromGregorian, secondsToDiffTime, Day, UTCTime(..) )
import Prettyprinter ( Pretty(..) )
import Witch ( into )
import Data.Set ( fromList, difference, Set )
-- end::import-declarations[]

main :: IO ()
main = do

  -- show Interval examples ----------------------------------------------------

  putStrLn "-- tag::interval-show-print[]"

  putStr "\n-- An example Interval Integer"
  putStr "\nprint ivInteger\n---> "
  print ivInteger

  putStr "\n-- An example Interval Day"
  putStr "\nprint ivDay\n---> "
  print ivDay

  putStr "\n-- An example Interval UTCTime"
  putStr "\nprint ivUTC\n---> "
  print ivUTC

  putStrLn "-- end::interval-show-print[]"


  -- Basic Interval instances examples ------------------------------------------

  putStrLn "-- tag::interval-basic-instances-print[]"

  putStr "\nprint $ ivInteger == ivInteger\n---> "
  print $ ivInteger == ivInteger

  putStr "\nprint $ ivDay < ivDay\n---> "
  print $ ivDay < ivDay

  putStr "\nprint $ show ivInteger\n---> "
  print $ show ivInteger

  putStr "\nprint $ fmap (+2) ivInteger\n---> "
  print $ fmap (+ 2) ivInteger

  putStrLn "-- end::interval-basic-instances-print[]"


  -- parseInterval examples ----------------------------------------------------

  putStrLn "-- tag::parseinterval-print[]"

  putStr "\nprint rightIvInteger\n---> "
  print rightIvInteger

  putStr "\nprint leftIvInteger\n---> "
  print leftIvInteger

  putStr "\nprint rightIvDay\n---> "
  print rightIvDay

  putStr "\nprint rightIvUTC\n---> "
  print rightIvUTC

  putStrLn "-- end::parseinterval-print[]"


  -- safeInterval examples -----------------------------------------------------

  putStrLn "-- tag::safeinterval-print[]"

  putStr "\nprint ivInteger\n---> "
  print ivInteger

  putStr "\nprint ivMinDurInteger\n---> "
  print ivMinDurInteger

  putStr "\nprint ivInteger\n---> "
  print ivInteger

  putStr "\nprint ivDay\n---> "
  print ivDay

  putStr "\nprint ivUTC\n---> "
  print ivUTC

  putStrLn "-- end::safeinterval-print[]"


  -- beginerval and enderval examples -------------------------------------------------------

  putStrLn "-- tag::beginerval-enderval-print[]"

  putStr "\nprint (beginerval 2 3 :: Interval Integer)\n---> "
  print (beginerval 2 3 :: Interval Integer)

  putStr "\nprint (beginerval (-2) 3 :: Interval Integer)\n---> "
  print (beginerval (-2) 3 :: Interval Integer)

  putStr "\nprint (enderval 2 12 :: Interval Integer)\n---> "
  print (enderval 2 12 :: Interval Integer)

  putStr "\nprint (enderval (-2) 12 :: Interval Integer)\n---> "
  print (enderval (-2) 12 :: Interval Integer)

  putStrLn "-- end::beginerval-enderval-print[]"


  -- Creating moments examples --------------------------------------------------

  putStrLn "-- tag::creating-moments-print[]"

  putStr "\nprint (beginervalMoment 11 :: Interval Integer)\n---> "
  print (beginervalMoment 11 :: Interval Integer)

  putStr "\nprint (endervalMoment 11 :: Interval Integer)\n---> "
  print (endervalMoment 11 :: Interval Integer)

  putStrLn "-- end::creating-moments-print[]"


  -- Creating PairedIntervals examples --------------------------------------

  putStrLn "-- tag::creating-pairedinterval-print[]"

  putStr "\nprint pairListstringInteger\n---> "
  print pairListstringInteger

  putStr "\nprint pairStringDay\n---> "
  print pairStringDay

  putStrLn "-- end::creating-pairedinterval-print[]"


  -- Basic PairedInterval instances examples ------------------------------------

  putStrLn "-- tag::pairedinterval-basic-instances-print[]"

  putStr "\nprint $ pairStringDay == pairStringDay\n---> "
  print $ pairStringDay == pairStringDay

  putStr "\nprint $ pairListstringInteger < pairListstringInteger\n---> "
  print $ pairListstringInteger < pairListstringInteger

  putStr "\nprint $ show pairStringDay\n---> "
  print $ show pairStringDay

  putStr "\nprint $ fmap (addDays 2) pairStringDay\n---> "
  print $ fmap (addDays 2) pairStringDay

  putStr "\nprint $ bimap (++ \" in summer\") (addDays 180) pairStringDay\n---> "
  print $ bimap (++ " in summer") (addDays 180) pairStringDay

  putStrLn "-- end::pairedinterval-basic-instances-print[]"


  -- Getting/setting PairedInterval intervals example --------------------------

  putStrLn "-- tag::pairedinterval-getset-intervals-print[]"

  putStr "\nprint pairListstringInteger\n---> "
  print pairListstringInteger

  putStr "\nprint $ getInterval pairListstringInteger\n---> "
  print $ getInterval pairListstringInteger

  putStr "\nprint $ setInterval pairListstringInteger (safeInterval (4, 9) :: Interval Integer)\n---> "
  print $ setInterval pairListstringInteger (safeInterval (4, 9) :: Interval Integer)

  putStr "\nprint $ intervals [pairListstringInteger, pairListstringInteger]\n---> "
  print $ intervals [pairListstringInteger, pairListstringInteger]

  putStr "\nprint $ begin pairListstringInteger\n---> "
  print $ begin pairListstringInteger

  putStr "\nprint $ end pairListstringInteger\n---> "
  print $ end pairListstringInteger

  putStrLn "-- end::pairedinterval-getset-intervals-print[]"


  -- Example getting/setting PairedInterval data -------------------------------

  putStrLn "-- tag::pairedinterval-getset-data-print[]"

  putStr "\nprint pairStringDay\n---> "
  print pairStringDay

  putStr "\nprint $ getPairData pairStringDay\n---> "
  print $ getPairData pairStringDay

  putStr "\nprint $ makePairedInterval \"ski trip\" (getInterval pairStringDay)\n---> "
  print $ makePairedInterval "ski trip" (getInterval pairStringDay)

  putStrLn "-- end::pairedinterval-getset-data-print[]"


  -- Intervallic interval instance examples ------------------------------------------------------

  putStrLn "-- tag::intervallic-interval-instance-print[]"

  putStr "\nprint ivInteger\n---> "
  print ivInteger

  putStr "\nprint $ getInterval ivInteger\n---> "
  print $ getInterval ivInteger

  putStr "\nprint $ setInterval ivInteger (beginerval 3 12 :: Interval Integer)\n---> "
  print $ setInterval ivInteger (beginerval 3 12 :: Interval Integer)

  putStr "\nprint $ begin ivInteger\n---> "
  print $ begin ivInteger

  putStr "\nprint $ end ivInteger\n---> "
  print $ end ivInteger

  putStrLn "-- end::intervallic-interval-instance-print[]"


  -- IntervalSizeable instance examples ------------------------------------------------------

  putStrLn "-- tag::intervalsizeable-instance-print[]"

  putStr "\nprint ivDay\n---> "
  print ivDay

  putStr "\nprint $ moment @Day\n---> "
  print $ moment @Day

  putStr "\nprint $ duration ivDay\n---> "
  print $ duration ivDay

  putStr "\nprint $ add 15 (begin ivDay)\n---> "
  print $ add 15 (begin ivDay)

  putStr "\nprint $ diff (add 15 (begin ivDay)) (begin ivDay)\n---> "
  print $ diff (add 15 (begin ivDay)) (begin ivDay)

  putStrLn "-- end::intervalsizeable-instance-print[]"


  -- IntervalCombineable Interval examples -------------------------------------

  putStrLn "-- tag::intervalcombinable-interval-print[]"

  putStrLn "\n-- The Just Interval formed from combining the Intervals, since iv0to2 `meets` iv2to5"
  putStr "print $ iv0to2 .+. iv2to5\n---> "
  print $ iv0to2 .+. iv2to5

  putStrLn "\n-- A Nothing since iv0to2 doesn't `meets` iv5to8"
  putStr "print $ iv0to2 .+. iv5to8\n---> "
  print $ iv0to2 .+. iv5to8

  putStrLn "\n-- The Just Interval formed from the end of the first and the beginning of the\n-- second, since iv0to2 is `before` iv5to8"
  putStr "print $ iv0to2 >< iv5to8\n---> "
  print $ iv0to2 >< iv5to8

  putStrLn "\n-- A Nothing since iv0to2 isn't `before` iv2to5"
  putStr "print $ iv0to2 >< iv2to5\n---> "
  print $ iv0to2 >< iv2to5

  putStrLn "\n-- A list with iv0to2 and iv3to6, since:"
  putStrLn "--     * iv0to2 is `before` iv3to6"
  putStrLn "--     * We have specified a list as the return type"
  putStrLn "--     * Lifting a value to a list returns a list with the input as the sole"
  putStrLn "--       element"
  putStrLn "--     * `<>` for lists concatenates the lists"
  putStr "print (iv0to2 <+> iv3to6 :: [Interval Integer])\n---> "
  print (iv0to2 <+> iv3to6 :: [Interval Integer])

  putStrLn "\n-- A list with one `Interval` spanning iv2to4 and iv3to6, since the former is\n-- not `before` the latter"
  putStr "print (iv2to4 <+> iv3to6 :: [Interval Integer])\n---> "
  print (iv2to4 <+> iv3to6 :: [Interval Integer])

  putStrLn "\n-- A Right with iv0to2, since:"
  putStrLn "--     * iv0to2 is `before` iv3to6"
  putStrLn "--     * We have specified Either as the return type"
  putStrLn "--     * Lifting a value to an Either returns a Right"
  putStrLn "--     * `<>` for two Rights returns the first Right"
  putStr "print (iv0to2 <+> iv3to6 :: Either () (Interval Integer))\n---> "
  print (iv0to2 <+> iv3to6 :: Either () (Interval Integer))

  putStrLn "\n-- A Right with one `Interval` spanning iv2to4 and iv3to6, since the former is\n-- not `before` the latter"
  putStr "print (iv2to4 <+> iv3to6 :: Either () (Interval Integer))\n---> "
  print (iv2to4 <+> iv3to6 :: Either () (Interval Integer))

  putStrLn "-- end::intervalcombinable-interval-print[]"


  -- IntervalCombineable PairedInterval examples -------------------------------

  putStrLn "-- tag::intervalcombinable-pairedinterval-print[]"

  putStrLn "\n-- The Just Interval formed from combining the Intervals and taking the data\n-- portion from the second argument, since iv0to2 `meets` iv2to5"
  putStr "print $ makePairedInterval \"a\" iv0to2 .+. makePairedInterval \"b\" iv2to5\n---> "
  print $ makePairedInterval "a" iv0to2 .+. makePairedInterval "b" iv2to5

  putStrLn "\n-- A Nothing since iv0to2 doesn't `meets` iv5to8"
  putStr "print $ makePairedInterval \"a\" iv0to2 .+. makePairedInterval \"b\" iv5to8\n---> "
  print $ makePairedInterval "a" iv0to2 .+. makePairedInterval "b" iv5to8

  putStrLn "\n-- The Just Interval formed from spanning the Intervals and taking the data\n-- portion from the `mempty` method of the Monoid String instance, since\n-- iv0to2 is `before` iv5to8"
  putStr "print $ makePairedInterval \"a\" iv0to2 >< makePairedInterval \"b\" iv5to8\n---> "
  print $ makePairedInterval "a" iv0to2 >< makePairedInterval "b" iv5to8

  putStrLn "\n-- A Nothing since iv0to2 isn't `before` iv2to5"
  putStr "print $ makePairedInterval \"a\" iv0to2 >< makePairedInterval \"b\" iv2to5\n---> "
  print $ makePairedInterval "a" iv0to2 >< makePairedInterval "b" iv2to5

  putStrLn "\n-- A list with elements (makePairedInterval \"a\" iv0to2) and \n-- (makePairedInterval \"b\" iv3to6), since:"
  putStrLn "--     * iv0to2 is `before` iv3to6"
  putStrLn "--     * We have specified a list as the return type"
  putStrLn "--     * Lifting a value to a list returns a list with the input as the sole"
  putStrLn "--       element"
  putStrLn "--     * `<>` for lists concatenates the lists"
  putStr "print (makePairedInterval \"a\" iv0to2 <+> makePairedInterval \"b\" iv3to6 :: [PairedInterval String Integer])\n--->"
  print (makePairedInterval "a" iv0to2 <+> makePairedInterval "b" iv3to6 :: [PairedInterval String Integer])

  putStrLn "\n-- A list with one `PairedInterval` where the embedded Interval spans iv2to4\n-- and iv3to6, and where the data is obtained by concatenating the embedded\n-- Strings, since:"
  putStrLn "--     * iv2to4 is not `before` iv3to6"
  putStrLn "--     * `<>` for Strings concatenates the Strings"
  putStrLn "--     * We have specified a list as the return type"
  putStrLn "--     * Lifting a value to a list returns a list with the input as the sole"
  putStrLn "--       element"
  putStr "print (makePairedInterval \"a\" iv2to4 <+> makePairedInterval \"b\" iv3to6 :: [PairedInterval String Integer])\n--->"
  print (makePairedInterval "a" iv2to4 <+> makePairedInterval "b" iv3to6 :: [PairedInterval String Integer])

  putStrLn "\n-- A Right with (makePairedInterval \"a\" iv0to2), since:"
  putStrLn "--     * iv0to2 is `before` iv3to6"
  putStrLn "--     * We have specified Either as the return type"
  putStrLn "--     * Lifting a value to an Either returns a Right"
  putStrLn "--     * `<>` for two Rights returns the first Right"
  putStr "print (makePairedInterval \"a\" iv0to2 <+> makePairedInterval \"b\" iv3to6 :: Either () (PairedInterval String Integer))\n---> "
  print (makePairedInterval "a" iv0to2 <+> makePairedInterval "b" iv3to6 :: Either () (PairedInterval String Integer))

  putStrLn "\n-- A Right with a `PairedInterval` where the embedded interval spans iv2to4\n-- and iv3to6, and where the data is obtained by concatenating the embedded\n-- Strings, since:"
  putStrLn "--     * iv2to4 is not `before` iv3to6"
  putStrLn "--     * `<>` for Strings concatenates the Strings"
  putStrLn "--     * We have specified Either as the return type"
  putStrLn "--     * Lifting a value to an Either returns a Right"
  putStr "print (makePairedInterval \"a\" iv2to4 <+> makePairedInterval \"b\" iv3to6 :: Either () (PairedInterval String Integer))\n---> "
  print (makePairedInterval "a" iv2to4 <+> makePairedInterval "b" iv3to6 :: Either () (PairedInterval String Integer))

  putStrLn "-- end::intervalcombinable-pairedinterval-print[]"


  -- Expanding intervals examples -----------------------------------------------

  putStrLn "-- tag::expanding-intervals-print[]"

  putStr "\nprint ivInteger\n---> "
  print ivInteger

  putStr "\nprint $ expandl 4 ivInteger\n---> "
  print $ expandl 4 ivInteger

  putStr "\nprint $ expandl 0 ivInteger\n---> "
  print $ expandl 0 ivInteger

  putStr "\nprint $ expandr 5 ivInteger\n---> "
  print $ expandr 5 ivInteger

  putStr "\nprint $ expandr (-3) ivInteger\n---> "
  print $ expandr (-3) ivInteger

  putStr "\nprint $ expand 4 5 ivInteger\n---> "
  print $ expand 4 5 ivInteger

  putStr "\nprint $ expand 0 (-3) ivInteger\n---> "
  print $ expand 0 (-3) ivInteger

  putStrLn "-- end::expanding-intervals-print[]"


  -- Sharing an endpoint interval examples -----------------------------------------------

  putStrLn "-- tag::sharing-endpoint-interval-print[]"

  putStr "\nprint ivInteger\n---> "
  print ivInteger

  putStr "\nbeginervalFromEnd 5 ivInteger\n---> "
  print $ beginervalFromEnd 5 ivInteger

  putStr "\nbeginervalFromEnd (-2) ivInteger\n---> "
  print $ beginervalFromEnd (-2) ivInteger

  putStr "\nendervalFromBegin 12 ivInteger\n---> "
  print $ endervalFromBegin 12 ivInteger

  putStr "\nendervalFromBegin (-6) ivInteger\n---> "
  print $ endervalFromBegin (-6) ivInteger

  putStr "\nprint $ momentize ivInteger\n---> "
  print $ momentize ivInteger

  putStrLn "-- end::sharing-endpoint-interval-print[]"


  -- Example shifting intervals -----------------------------------------------

  putStrLn "-- tag::shifting-intervals-print[]"

  putStr "\nprint [iv2to4, iv5to8]\n---> "
  print [iv2to4, iv5to8]

  putStr "\nprint ivDay\n---> "
  print ivDay

  putStr "\nprint $ shiftFromBegin iv2to4 (Just 9)\n---> "
  print $ shiftFromBegin iv2to4 (Just 9)

  putStr "\nprint $ shiftFromBegin iv2to4 iv5to8\n---> "
  print $ shiftFromBegin iv2to4 iv5to8

  putStr "\nprint $ shiftFromBegin ivDay ivDay\n---> "
  print $ shiftFromBegin ivDay ivDay

  putStr "\nprint $ shiftFromEnd iv2to4 (Just 9)\n---> "
  print $ shiftFromEnd iv2to4 (Just 9)

  putStr "\nprint $ shiftFromEnd iv2to4 iv5to8\n---> "
  print $ shiftFromEnd iv2to4 iv5to8

  putStr "\nprint $ shiftFromEnd ivDay ivDay\n---> "
  print $ shiftFromEnd ivDay ivDay

  putStrLn "-- end::shifting-intervals-print[]"


  -- Relations variables diagram example --------------------------------------------

  putStrLn "-- tag::relations-variables-diagram-print[]"

  print $ pretty diagr

  putStrLn "-- end::relations-variables-diagram-print[]"


  -- Composing relations examples -------------------------------------------

  putStrLn "-- tag::composing-relations-print[]"

  putStr "\nprint endedPriorRelations\n---> "
  print endedPriorRelations

  putStr "\nprint notEndedPriorRelations\n---> "
  print notEndedPriorRelations

  putStr "\nprint notEndedPriorRelations'\n---> "
  print notEndedPriorRelations'

  putStr "\nprint intervalRelations'\n---> "
  print intervalRelations'

  putStr "\nprint empty\n---> "
  print empty

  putStrLn "-- end::composing-relations-print[]"


  -- Composing predicates v1 examples ---------------------------------------

  putStrLn "-- tag::composing-predicates-1-print[]"

  putStr "\nprint $ iv0to2 `precedes` iv3to6"
  putStr "\nprint $ iv0to2 `meets` iv3to6"
  putStr "\nprint $ iv0to2 `endedPrior` iv3to6\n---> "
  print $ iv0to2 `precedes` iv3to6
  putStr "---> "
  print $ iv0to2 `meets` iv3to6
  putStr "---> "
  print $ iv0to2 `endedPrior` iv3to6

  putStrLn "-- end::composing-predicates-1-print[]"


  -- Composing predicates v2 examples ---------------------------------------

  putStrLn "-- tag::composing-predicates-2-print[]"

  putStr "\nprint $ iv0to2 `precedes` iv2to4"
  putStr "\nprint $ iv0to2 `meets` iv2to4"
  putStr "\nprint $ iv0to2 `endedPrior` iv2to4\n---> "
  print $ iv0to2 `precedes` iv2to4
  putStr "---> "
  print $ iv0to2 `meets` iv2to4
  putStr "---> "
  print $ iv0to2 `endedPrior` iv2to4

  putStrLn "-- end::composing-predicates-2-print[]"


  -- Composing predicates v3 examples ---------------------------------------

  putStrLn "-- tag::composing-predicates-3-print[]"

  putStr "\nprint $ iv5to8 `precedes` iv2to4"
  putStr "\nprint $ iv5to8 `meets` iv2to4"
  putStr "\nprint $ iv5to8 `endedPrior` iv2to4\n---> "
  print $ iv5to8 `precedes` iv2to4
  putStr "---> "
  print $ iv5to8 `meets` iv2to4
  putStr "---> "
  print $ iv5to8 `endedPrior` iv2to4

  putStrLn "-- end::composing-predicates-3-print[]"


  -- Extended example 1 examples --------------------------------------------------------

  putStrLn "-- tag::extended-example-1-print[]"

  putStr "\nprint $ head results\n---> "
  print $ head results

  putStr "\nprint $ results !! 1\n---> "
  print $ results !! 1

  putStr "\nprint $ results !! 2\n---> "
  print $ results !! 2

  putStr "\nprint $ results !! 3\n---> "
  print $ results !! 3

  putStrLn "-- end::extended-example-1-print[]"


-- tag::parseinterval-examples[]
rightIvInteger :: Either ParseErrorInterval (Interval Integer)
rightIvInteger = parseInterval 0 2

leftIvInteger :: Either ParseErrorInterval (Interval Integer)
leftIvInteger = parseInterval 2 2

rightIvDay :: Either ParseErrorInterval (Interval Day)
rightIvDay =
  parseInterval (fromGregorian 1967 01 18) (fromGregorian 1967 01 22)

rightIvUTC :: Either ParseErrorInterval (Interval UTCTime)
rightIvUTC = parseInterval
  (UTCTime (fromGregorian 1967 01 18) (secondsToDiffTime 32400))
  (UTCTime (fromGregorian 1967 01 18) (secondsToDiffTime 33200))
-- end::parseinterval-examples[]


-- tag::safeinterval-examples[]
ivInteger :: Interval Integer
ivInteger = safeInterval (2, 6)

ivMinDurInteger :: Interval Integer
ivMinDurInteger = safeInterval (2, 2)

ivDay :: Interval Day
ivDay = safeInterval (fromGregorian 1967 01 18, fromGregorian 1967 01 24)

ivUTC :: Interval UTCTime
ivUTC = safeInterval
  ( UTCTime (fromGregorian 1967 01 18) (secondsToDiffTime 32400)
  , UTCTime (fromGregorian 1967 01 18) (secondsToDiffTime 33200)
  )
-- end::safeinterval-examples[]


-- tag::ivXtoY-examples[] ----------------

iv0to2, iv2to4, iv2to5, iv4to5, iv5to8, iv6to8, iv3to6 :: Interval Integer
iv0to2 = safeInterval (0, 2)
iv2to4 = safeInterval (2, 4)
iv2to5 = safeInterval (2, 5)
iv3to6 = safeInterval (3, 6)
iv4to5 = safeInterval (4, 5)
iv5to8 = safeInterval (5, 8)
iv6to8 = safeInterval (6, 8)

-- end::ivXtoY-examples[]


-- tag::creating-pairedinterval-examples[] --------------------

pairListstringInteger :: PairedInterval [String] Integer
pairListstringInteger =
  makePairedInterval ["John", "Paul", "George", "Ringo"] ivInteger

pairStringDay :: PairedInterval String Day
pairStringDay = makePairedInterval "vacation" ivDay

-- end::creating-pairedinterval-examples[]


-- tag::composing-relations-examples[]

-- Set, `fromList`, and in a later example `difference` are imported from
-- Data.Set
endedPriorRelations :: Set IntervalRelation
endedPriorRelations = fromList [Before, Meets]

-- We can in general create a new Set by taking the set Difference of one Set
-- and another Set
notEndedPriorRelations :: Set IntervalRelation
notEndedPriorRelations = intervalRelations `difference` endedPriorRelations

-- However, the `complement` function is provided for the common case of taking
-- the Set Difference of the `intervalRelations` Set and another Set
notEndedPriorRelations' :: Set IntervalRelation
notEndedPriorRelations' = complement endedPriorRelations

-- IntervalAlgebra exports versions of `Data.Set`s `intersection` and `union`
-- functions where the types are specialized to `Set IntervalRelation`s
intervalRelations' :: Set IntervalRelation
intervalRelations' = endedPriorRelations `union` notEndedPriorRelations

-- The intersection of two disjoint sets
empty :: Set IntervalRelation
empty = endedPriorRelations `intersection` notEndedPriorRelations

-- end::composing-relations-examples[]


-- tag::composing-predicates-examples[] -----------------------

-- We can construct a predicate function from a 'Set IntervalRelation'
endedPrior
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
endedPrior = predicate (fromList [Before, Meets])

-- We can also construct a predicate function directly from a list of predicate
-- functions
endedPrior'
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
endedPrior' = unionPredicates [before, meets]

-- As an alternative to `unionPredicates` we can compose predicate functions
-- using the <|> operator. If we had multiple predicates we could use e.g.:
--     p1 <|> p2 <|> p3
endedPrior''
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
endedPrior'' = before <|> meets

-- end::composing-predicates-examples[]


-- tag::extended-example-1-types[] --------------------

data DataType = Enrollment | Treatment TreatmentType | Diagnosis DiagnosisType
  deriving (Eq, Ord, Show)

data TreatmentType = StandardVaccine | NewVaccine
  deriving (Eq, Ord, Show)

data DiagnosisType = RightAsRain | UpsetTummy | CommonCold | Flu
  deriving (Eq, Ord, Show)

type StudyEvent = PairedInterval DataType Integer

type SubjEvents = [StudyEvent]

data ProcessedSubj = ProcessedSubj
  { getEnrollment   :: Maybe (Interval Integer)
  , getFirstTrt     :: Maybe StudyEvent
  , getFirstFlu     :: Maybe StudyEvent
  , getTrtType      :: Maybe TreatmentType
  , getTimeToFlu    :: Maybe Integer
  , getTimeToEndEnr :: Maybe Integer
  }
  deriving Show

maxEnrGap :: Integer
maxEnrGap = 8

-- end::extended-example-1-types[]


-- tag::extended-example-1-study-data-subj-1[] ----------------

id1Event1, id1Event2, id1Event3, id1Event4, id1Event5, id1Event6, id1Event7
  :: StudyEvent
id1Event1 = makePairedInterval Enrollment (safeInterval (6, 191))
id1Event2 = makePairedInterval Enrollment (safeInterval (199, 345))
id1Event3 = makePairedInterval Enrollment (safeInterval (347, 422))
id1Event4 = makePairedInterval (Diagnosis RightAsRain) (safeInterval (12, 13))
id1Event5 = makePairedInterval (Treatment NewVaccine) (safeInterval (22, 23))
id1Event6 = makePairedInterval (Diagnosis RightAsRain) (safeInterval (131, 132))
id1Event7 = makePairedInterval (Diagnosis CommonCold) (safeInterval (161, 162))

id1Events :: SubjEvents
id1Events = sort
  [id1Event1, id1Event2, id1Event3, id1Event4, id1Event5, id1Event6, id1Event7]

-- end::extended-example-1-study-data-subj-1[]


-- tag::extended-example-1-study-data-subj-2[] ----------------

id2Event1, id2Event2, id2Event3, id2Event4, id2Event5, id2Event6, id2Event7, id2Event8
  :: StudyEvent
id2Event1 = makePairedInterval Enrollment (safeInterval (2, 206))
id2Event2 = makePairedInterval Enrollment (safeInterval (222, 299))
id2Event3 = makePairedInterval Enrollment (safeInterval (304, 486))
id2Event4 = makePairedInterval (Diagnosis RightAsRain) (safeInterval (4, 5))
id2Event5 =
  makePairedInterval (Treatment StandardVaccine) (safeInterval (98, 99))
id2Event6 = makePairedInterval (Diagnosis CommonCold) (safeInterval (161, 162))
id2Event7 = makePairedInterval (Diagnosis UpsetTummy) (safeInterval (191, 192))
id2Event8 = makePairedInterval (Diagnosis Flu) (safeInterval (255, 256))

id2Events :: SubjEvents
id2Events = sort
  [ id2Event1, id2Event2, id2Event3, id2Event4, id2Event5 , id2Event6
  , id2Event7, id2Event8 ]

-- end::extended-example-1-study-data-subj-2[]


-- tag::extended-example-1-study-data-subj-3[] ----------------

id3Event1, id3Event2, id3Event3 :: StudyEvent
id3Event1 = makePairedInterval Enrollment (safeInterval (7, 197))
id3Event2 = makePairedInterval (Treatment StandardVaccine) (safeInterval (19, 20))
id3Event3 = makePairedInterval (Diagnosis Flu) (safeInterval (180, 181))

id3Events :: SubjEvents
id3Events = sort [id3Event1, id3Event2, id3Event3]

-- end::extended-example-1-study-data-subj-3[]


-- tag::extended-example-1-study-data-subj-4[] ----------------

id4Event1, id4Event2 :: StudyEvent
id4Event1 = makePairedInterval Enrollment (safeInterval (3, 89))
id4Event2 = makePairedInterval (Diagnosis RightAsRain) (safeInterval (47, 48))

id4Events :: SubjEvents
id4Events = sort [id4Event1, id4Event2]

-- end::extended-example-1-study-data-subj-4[]


-- tag::extended-example-1-processing-functions[] -------------

-- Construct the elements of a `ProcessedSubj` one step at-a-time. Most of the
-- actual work is done by the helper functions defined below
processSubj :: SubjEvents -> ProcessedSubj
processSubj xs =
  let enrPeriod    = calcEnrPeriod xs                   -- enrollment period
      enrEvents    = calcEnrEvents enrPeriod xs         -- events within enrollment
      firstTrt     = findFirstTrt enrPeriod xs          -- first treatment in enr
      firstTrtType = extractTrtType firstTrt            -- first trt type
      firstTrtIv   = fmap getInterval firstTrt          -- first trt interval
      firstFlu     = findFirstFlu firstTrtIv enrEvents  -- first flu in enr
      ttFlu        = calcDiff firstFlu firstTrt         -- time to first flu
      ttEndEnr     = calcAtRisk enrPeriod firstTrt      -- time to end of enr
  in  ProcessedSubj enrPeriod firstTrt firstFlu firstTrtType ttFlu ttEndEnr

-- Construct the "enrollment period", which is defined at the period of time
-- with the start endpoint given by their earliest enrollment period and end
-- endpoint given by the first time that they fall out of the grace period. In
-- the event that a subject did not have any enrollment periods then the return
-- value is a Nothing.
--
-- Note that this function uses the `combineIntervals` function which was not
-- covered in this tutorial, but is exported from IntervalAlgebra via
-- IntervalAlgebra.IntervalUtilities.
calcEnrPeriod :: SubjEvents -> Maybe (Interval Integer)
calcEnrPeriod xs | null combinedPeriods = Nothing
                 | otherwise            = Just (head combinedPeriods)
  where combinedPeriods = (combineIntervals . addMaxEnrGap . extractEnrIvs) xs

-- Filter the enrollment events in the SubjEvents and extract the Interval
-- from each one
extractEnrIvs :: SubjEvents -> [Interval Integer]
extractEnrIvs = intervals . filter (checkEnr . getPairData)
 where
  checkEnr Enrollment = True
  checkEnr _          = False

-- Extend the end endpoint for each Interval by `maxEnrGap`
addMaxEnrGap :: [Interval Integer] -> [Interval Integer]
addMaxEnrGap = map (expandr maxEnrGap)

-- Filter the SubjEvents to those with endpoints that do not extend past the
-- enrollment period's endpoints
calcEnrEvents :: Maybe (Interval Integer) -> SubjEvents -> SubjEvents
calcEnrEvents mayIv xs = case mayIv of
  Nothing -> []
  Just y  -> filter (\x -> getInterval x `enclosedBy` y) xs

-- Find the first flu vaccine administrations occuring within the enrollment
-- period
findFirstTrt :: Maybe (Interval Integer) -> SubjEvents -> Maybe StudyEvent
findFirstTrt Nothing _ = Nothing
findFirstTrt (Just iv) xs | null filteredIntakes = Nothing
                          | otherwise            = Just (head filteredIntakes)
 where
  p x = checkTrt (getPairData x) && (getInterval x `enclosedBy` iv)
  checkTrt (Treatment _) = True
  checkTrt _             = False
  filteredIntakes = filter p xs

-- Extract the treatment type out of a StudyEvent. If there is no event or the
-- StudyEvent type isn't TreatmentType then return Nothing
extractTrtType :: Maybe StudyEvent -> Maybe TreatmentType
extractTrtType Nothing  = Nothing
extractTrtType (Just x) = case getPairData x of
  Treatment StandardVaccine -> Just StandardVaccine
  Treatment NewVaccine      -> Just NewVaccine
  _                         -> Nothing

-- Find the first flu diagnosis occuring within the enrollment period
findFirstFlu :: Maybe (Interval Integer) -> SubjEvents -> Maybe StudyEvent
findFirstFlu Nothing _ = Nothing
findFirstFlu (Just iv) xs | null filteredFlus = Nothing
                          | otherwise         = Just (head filteredFlus)
 where
  endedPrior = before <|> meets
  p x = checkDiagFlu (getPairData x) && (iv `endedPrior` getInterval x)
  checkDiagFlu (Diagnosis Flu) = True
  checkDiagFlu _               = False
  filteredFlus = filter p xs

-- Calculate the difference between the start endpoint of the first Intervallic
-- and the start endpoint of the second Intervallic
calcDiff
  :: (IntervalSizeable a b, Intervallic i0, Intervallic i1)
  => Maybe (i0 a)
  -> Maybe (i1 a)
  -> Maybe b
calcDiff (Just y) (Just x) = Just $ diff (begin y) (end x)
calcDiff _ _               = Nothing

-- Calculate the difference between the end endpoint of the first Intervallic
-- and the start endpoint of the second Intervallic
calcAtRisk
  :: (IntervalSizeable a b, Intervallic i0, Intervallic i1)
  => Maybe (i0 a)
  -> Maybe (i1 a)
  -> Maybe b
calcAtRisk (Just y) (Just x) = Just $ diff (end y) (end x)
calcAtRisk _ _               = Nothing

-- end::extended-example-1-processing-functions[]


-- tag::extended-example-1-calculate-results[] ----------------

results :: [ProcessedSubj]
results = map processSubj [id1Events, id2Events, id3Events, id4Events]

-- end::extended-example-1-calculate-results[]


--------------------------------------------------------------------------------
-- The remaining is untagged data
--------------------------------------------------------------------------------

diagr :: Either IntervalDiagramParseError (IntervalDiagram Integer)
diagr = parseIntervalDiagram
  defaultIntervalDiagramOptions
  []
  (Just Bottom)
  (into @(IntervalText Integer) ('=', safeInterval (0, 12 :: Integer)))
  [ ([deftIT iv0to2], ["iv0to2"])
  , ([deftIT iv2to4], ["iv2to4"])
  , ([deftIT iv2to5], ["iv2to5"])
  , ([deftIT iv3to6], ["iv3to6"])
  , ([deftIT iv4to5], ["iv4to5"])
  , ([deftIT iv6to8], ["iv6to8"])
  , ([deftIT iv5to8], ["iv5to8"])
  ]

intoIntervalText :: Interval Integer -> IntervalText Integer
intoIntervalText x = into @(IntervalText Integer) ('-', x)

deftIT :: Interval Integer -> IntervalText Integer
deftIT = intoIntervalText
