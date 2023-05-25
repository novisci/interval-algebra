{-|
Module      : Interval Algebra
Description : Implementation of Allen's interval algebra
Copyright   : (c) NoviSci, Inc 2020-2022
                  TargetRWE, 2023
License     : BSD3
Maintainer  : bsaul@novisci.com 2020-2022
              bbrown@targetrwe.com 2023

The @IntervalAlgebra@ module provides data types and related classes for the
interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434)
and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

This main module reexports @IntervalAlgebra.Core@, @IntervalAlgebra.IntervalUtilities@,
and @IntervalAlgebra.PairedInterval@, which is probably more than enough to get
going for most cases.

-}

module IntervalAlgebra
  ( module IntervalAlgebra.Core
  , module IntervalAlgebra.IntervalUtilities
  , module IntervalAlgebra.PairedInterval
  ) where

import           IntervalAlgebra.Core
import           IntervalAlgebra.IntervalDiagram
import           IntervalAlgebra.IntervalUtilities
import           IntervalAlgebra.PairedInterval
