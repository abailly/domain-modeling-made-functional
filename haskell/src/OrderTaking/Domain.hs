{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module OrderTaking.Domain where

import Basement.Bounded (Zn)
import Data.Fixed (Centi, Fixed)
import Data.Text (Text)
import Data.Void (Void)
import GHC.TypeLits (Nat)
import Numeric.Natural (Natural)

-- | Constraint in book says:
-- > starts with W and 4 digits
-- We could hide the /constructor/ and only export the type, providing a
-- smart constructor to ensure well-formedness, but is this not kind of
-- cheating with the whole idea of type-driven domain modeling?
--
-- So let's be more explicit in Haskell:
-- * We need a type expressing the constraint that we have 4 digits, but 4 Digits
--   is just a number between 0 and 9999, so let's use `Zn` type to provide type-level
--   constraint
-- * We can leave the `W` rule out as this is just an external representation
--   and provide either `Show` and `Read` instances, or better yet serialisation
--   functions that will be reusable elsewhere (eg. in JSON, DB...)
newtype WidgetCode = WidgetCode (SmallerThan 10000)
  deriving (Eq, Show)

-- | Constraint in book says:
-- > starts with G and 3 digits
newtype GizmoCode = GizmoCode (SmallerThan 1000)
  deriving (Eq, Show)

type SmallerThan = Zn

data ProductCode = Widget WidgetCode | Gizmo GizmoCode
  deriving (Eq, Show)

newtype UnitQuantity = UnitQuantity Natural
  deriving (Eq, Show)

newtype KilogramQuantity = KilogramQuantity Centi
  deriving (Eq, Show)

data OrderQuantity
  = Unit UnitQuantity
  | Kilos KilogramQuantity
  deriving (Eq, Show)

-- We don't know yet what this ID and other identifiers and structures look like
-- The only possible value for `Void` is `undefined` so we have the guarantee we
-- will never be able to build any of these
newtype OrderId = OrderId Void

newtype OrderLineId = OrderLineId Void

newtype CustomerId = CustomerId Void

newtype CustomerInfo = CustomerInfo Void

newtype ShippingAddress = ShippingAddress Void

newtype BillingAddress = BillingAddress Void

newtype Price = Price Void

newtype BillingAmount = BillingAmount Void

data Order = Order
  { id :: OrderId,
    customerId :: CustomerId,
    shippingAddress :: ShippingAddress,
    billingAddress :: BillingAddress,
    orderLines :: [OrderLine],
    amountToBill :: BillingAmount
  }

data OrderLine = OrderLine
  { id :: OrderLineId,
    -- | Why is this here? There is no associated constrainter
    -- telling us this should be the same as the enclosing order's id
    -- Furthermore an OrderLine is meaningless outside of the Order (bouned) context
    -- so will always be accessed and manipulated through an Order
    orderId :: OrderId,
    productCode :: ProductCode,
    orderQuantity :: OrderQuantity,
    price :: Price
  }

data UnvalidatedOrder = UnvalidatedOrder
  { orderId :: OrderId,
    customerInfo :: CustomerInfo,
    shippingAddress :: ShippingAddress
  }

data PlaceOrderEvents = PlaceOrderEvents
  { acknowledgmentSent :: AcknowledgmentSent,
    orderPlaced :: OrderPlaced,
    billableOrderPlaced :: BillableOrderPlaced
  }

newtype AcknowledgmentSent = AcknowledgmentSent Void

newtype OrderPlaced = OrderPlaced Void

newtype BillableOrderPlaced = BillableOrderPlaced Void

newtype PlaceOrderError = ValidationErrors [ValidationError]

data ValidationError = ValidationError {fieldName :: Text, errorDescription :: Text}

-- | The type of `PlaceOrder` workflow.
-- In Haskell, the `Result` type is traditionally `Either` and the "error" is on the `Left`
-- side with the result on the `Right` side.
type PlaceOrder = UnvalidatedOrder -> Either PlaceOrderError PlaceOrderEvents
