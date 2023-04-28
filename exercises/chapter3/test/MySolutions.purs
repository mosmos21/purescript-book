module Test.MySolutions where

import Prelude
import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.Maybe (Maybe, isJust)
import Data.List (filter, head, nubByEq)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter findStreet
    where
        findStreet :: Entry -> Boolean
        findStreet entry = entry.address.street == street


isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = isJust <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq compareAddressBook
    where
        compareAddressBook :: Entry -> Entry -> Boolean
        compareAddressBook e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
