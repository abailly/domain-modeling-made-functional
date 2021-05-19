module OrderTaking.Chapter5.AsString where

-- In order to encode/decode our "codes" we need to provide a  pair of functions to represent
-- a code as a String and transform its string representation into a proper value
class AsString code where
  -- | Transform a `code` into a `String`.
  -- Note this function is total, it cannot fail.
  toString :: code -> String

  -- | Make a `WidgetCode` from its string representation
  -- This can fail if the code is invalid, in which case we return an error from the domain
  fromString :: String -> Either ValidationError code

integerDigits :: Int -> Parsec String () Integer
integerDigits numDigits = do
  s <- count numDigits digit
  maybe (fail $ "can't parse " <> s <> " as a number") pure $ readNumber s

readNumber :: String -> Maybe Integer
readNumber s =
  case reads s of
    [(m, [])] -> Just m
    _ -> Nothing
