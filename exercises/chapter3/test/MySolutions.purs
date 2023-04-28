module Test.MySolutions where

import Prelude
import Data.AddressBook (Address, Entry, AddressBook)
import Data.Maybe (Maybe)
import Data.List (filter, head)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter findStreet
    where
        findStreet :: Entry -> Boolean
        findStreet entry = entry.address.street == street
