{-|
Module      : Interval Algebra
Description : Implementation of Allen's interval algebra
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

The @IntervalAlgebra@ module provides data types and related classes for the 
interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434)
and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x). 
A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

This main module reexports @IntervalAlgebra.Core@, @IntervalAlgebra.IntervalUtilities@,
and @IntervalAlgebra.PairedInterval@, which is probably more than enough to get
going for most cases.

-}

{-# LANGUAGE Safe #-}
module IntervalAlgebra
  ( module IntervalAlgebra.Core
  , module IntervalAlgebra.IntervalUtilities
  , module IntervalAlgebra.PairedInterval
  ) where

import safe      IntervalAlgebra.Core
import safe      IntervalAlgebra.IntervalUtilities
import safe      IntervalAlgebra.PairedInterval
