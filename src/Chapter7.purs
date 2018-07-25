module Chapter7 where

import Data.AddressBook
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fx) = Just <$> fx

type Errors = Array String

matches :: String -> Regex -> String -> V Errors Unit
matches _ regex value | test regex value = pure unit
matches field _ _ = invalid ["Field '" <> field <> 
                    "' did not match the required format"]

whitespaceRegex :: Regex
whitespaceRegex =
  unsafePartial
    case regex "^[\\s]+$" noFlags of
      Right r -> r

nonWhitespace :: String -> String -> V Errors Unit
nonWhitespace field "" = invalid ["Field " <> field <> " cannot be empty"]
nonWhitespace field value | test whitespaceRegex value = 
  invalid ["Field " <> field <> " cannot be whitespace"]
nonWhitespace _ _ = pure unit

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[a-zA-Z]{2}$" noFlags of
      Right r -> r

validateState :: String -> V Errors Unit
validateState text =
  case test stateRegex text of
    true -> pure unit
    false -> invalid ["Field state must be two alphabetic characters"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonWhitespace "Street" o.street *> pure o.street)
          <*> (nonWhitespace "City"   o.city   *> pure o.city)
          <*> (validateState o.state *> pure o.state)


