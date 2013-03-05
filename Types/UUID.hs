{- git-annex UUID type
 -
 - Copyright 2011,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.UUID (
	UUID,
	UUIDMap,
	fromUUID,
	toUUID,
	genUUID
) where

import qualified Data.Map as M
import qualified Data.UUID as U
import System.Random
import Control.Applicative

-- A UUID an arbitrary opaque string, which cannot be empty.
data UUID = UUID String
	deriving (Eq, Ord, Show, Read)

type UUIDMap = M.Map UUID String

fromUUID :: UUID -> String
fromUUID (UUID u) = u

toUUID :: String -> Maybe UUID
toUUID [] = Nothing
toUUID s = Just (UUID s)

{- Generates a random UUID, that does not include the MAC address. -}
genUUID :: IO UUID
genUUID = UUID . show <$> (randomIO :: IO U.UUID)
