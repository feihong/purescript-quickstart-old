module Chapter3 where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)
import Data.String (toLower)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

createEntry :: String -> String -> Address -> Entry
createEntry = {firstName: _, lastName: _, address: _}

createAddress :: String -> String -> String -> Address
createAddress = {street: _, city: _, state: _}

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = filter (\e -> e.address == address) >>> head

nameAppears :: String -> AddressBook -> Boolean
nameAppears name = filter fn >>> null >>> not
  where
    fn :: Entry -> Boolean
    fn e =
      let name' = toLower name
      in (toLower e.firstName) == name' || (toLower e.lastName) == name'

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = 
  nubBy (\a b -> a.firstName == b.firstName && a.lastName == b.lastName)