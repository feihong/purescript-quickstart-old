module Chapter7 where

import Data.AddressBook
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)
import Data.String as S

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fx) = Just <$> fx

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | S.length value /= len =
  invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     =
  pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[A-Z]{2}$" noFlags of
      Right r -> r

