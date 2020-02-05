module ErrorHandling (

      extractValue
    , trapError

    ) where

import Control.Monad ( return )
import Control.Monad.Except ( catchError )

import LispTypes ( ThrowsError(..) )

import Text.Show ( show )

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = catchError action (return . show)
