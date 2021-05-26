{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module OrderTaking.Chapter7.Domain where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Void (Void)
import OrderTaking.Chapter5.Domain hiding (BillableOrderPlaced, Order (..), OrderLine (..), OrderPlaced, UnvalidatedOrder)

data Command a = Command
  { command :: a,
    timestamp :: UTCTime,
    -- | A user identifier
    -- It's a `String` in the book which sucks
    userId :: UserId
  }

newtype UserId = UserId Void

-- | The states of the `Order` lifecycle
-- We can use them as types to annotate the lifecycle status of an
-- existing piece of data
data OrderState = Unvalidated | Validated | Priced

-- | A type attaching some state to another type
-- The use of `newtype` ensures there's no runtime overhead and the
-- representations are identical.
newtype HasState (state :: k) a = HasState a

-- | a GADT to track the lifecycle of a loan
-- The fact each constructor is associated with a different type makes it obvious
-- what's the representation for each state. This allows to carry the actual
-- status using a concrete "witness" of the type, eg. a concrete value with a
-- specific constructor.
data Order (status :: OrderState) where
  OrderUnvalidated :: RawOrder Unvalidated -> Order Unvalidated
  OrderValidated :: RawOrder Validated -> Order Validated
  OrderPriced :: PricedOrder -> Order Priced

-- | A validated order is just the same as an unvalidated one, so better track this Validation
-- process at the type-level using a phantom type.
data RawOrder valid = RawOrder
  { id :: OrderId,
    -- | we wrap the `CustomerInfo` and other fields into a newtype so the underlying representation
    -- stays the same but we can track the validation status of the object
    customerInfo :: HasState valid CustomerInfo,
    shippingAddress :: HasState valid Address,
    billingAddress :: HasState valid Address,
    orderLines :: [HasState valid OrderLine]
  }

data OrderLine = OrderLine
  { id :: OrderLineId,
    orderId :: OrderId,
    productCode :: ProductCode,
    orderQuantity :: OrderQuantity,
    price :: Price
  }

data PricedOrder = PricedOrder
  { orderId :: OrderId,
    customerInfo :: CustomerInfo,
    shippingAddress :: Address,
    billingAddress :: Address,
    orderLines :: [PricedOrderLine],
    amountToBill :: BillingAmount
  }

-- | this is defined on p.39 in the domain model
data PricedOrderLine = PricedOrderLine
  { validatedOrderLine :: HasState Validated OrderLine,
    linePrice :: Price
  }

-- * Validation Step modelling

-- | Step to validate an order
-- This is where Haskell's emphasis on purity kicks in: We simply can't assume the dependencies
-- will be pure functions anymore and we need to express the fact they will need some side-effects,
-- talking to a DB or to a remote service.
-- That's what the `effect` parameter models, we leave it unspecified for now.
type ValidateOrder effect = Order Unvalidated -> CheckProductCodeExists effect -> CheckAddressExists effect -> effect (Either ValidationError (Order Validated))

type CheckProductCodeExists effect = ProductCode -> effect Bool

type CheckAddressExists effect = HasState Unvalidated Address -> effect (Either AddressValidationError (HasState Validated Address))

type AddressValidationError = Void

-- * Pricing Step

-- | Price an order
-- I guess the function is supposed to always succeed because the order has been validated
-- so we have the guarantee it can be priced?
type PriceOrder effect = Order Validated -> GetProductPrice effect -> effect (Order Priced)

type GetProductPrice effect = ProductCode -> effect Price

-- * Acknowledge Order Step

newtype HtmlString = HtmlString Text

data OrderAcknowledgment = OrderAcknowledgment
  { emailAddress :: EmailAddress,
    letter :: HtmlString
  }

type CreateOrderAcknowledgmentLetter = Order Priced -> HtmlString

type SendOrderAcknowledgment effect = OrderAcknowledgment -> effect SendResult

data SendResult = Sent | NotSent

type AcknowledgeOrder effect = CreateOrderAcknowledgmentLetter -> SendOrderAcknowledgment effect -> Order Priced -> Maybe OrderAcknowledgmentSent

-- * Events

data PlaceOrderEvent
  = Acknowledged OrderAcknowledgmentSent
  | Placed OrderPlaced
  | BillablePlaced BillableOrderPlaced

data OrderAcknowledgmentSent = OrderAcknowledgmentSent
  { orderId :: OrderId,
    emailAddress :: EmailAddress
  }

type OrderPlaced = Order Priced

data BillableOrderPlaced = BillableOrderPlaced
  { orderId :: OrderId,
    billingAddress :: Address,
    amountToBill :: BillingAmount
  }

type CreateEvents = Order Priced -> [PlaceOrderEvent]

-- | The type of `PlaceOrder` workflow.
-- In Haskell, the `Result` type is traditionally `Either` and the "error" is on the `Left`
-- side with the result on the `Right` side.
type PlaceOrderWorkflow effect = Command (Order Unvalidated) -> effect (Either PlaceOrderError [PlaceOrderEvent])
