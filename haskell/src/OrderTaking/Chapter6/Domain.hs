{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module OrderTaking.Chapter6.Domain
  ( ValidationStatus (..),
    IsValid,
    Validate,
    Valid,
    Invalid,
  )
where

import Data.Void (Void)
import Numeric.Natural (Natural)

-- | A type used as a /kind/ stating whether or not some
-- other type is validated.
data ValidationStatus = Validated | Unvalidated

-- | A newtype attaching `ValidationStatus` to some other type.
-- The use of `newtype` ensures there's no runtime overhead and the
-- representations are identical.
newtype IsValid (status :: ValidationStatus) a
  = IsValid a

type ValidationError = Void

type Valid a = IsValid Validated a

type Invalid a = IsValid Unvalidated a

-- | A generic validation function type that defines how one validates some
-- value.
type Validate a = Invalid a -> Either ValidationError (Valid a)

newtype Address = Address Void

-- | An `Order` can be valid or invalid, depending on the status of contained addresses.
newtype Order (valid :: ValidationStatus) = Order {shippingAddress :: IsValid valid Address}

data Unit = Gram | Decigram | Kilogram | Ton

newtype Weight (unit :: Unit) = Weight Double

data OrderLine = OrderLine {id :: String, weight :: Weight Kilogram}
