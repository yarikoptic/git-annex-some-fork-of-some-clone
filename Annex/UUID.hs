{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.UUID (
	getUUID,
	getRepoUUID,
	getUncachedUUID,
	ensureUUID,
	genUUID,
	removeRepoUUID,
	storeUUID,
) where

import Common.Annex
import qualified Git
import qualified Git.Config
import Config
import Types.UUID

configkey :: ConfigKey
configkey = annexConfig "uuid"

{- Get current repository's UUID. -}
getUUID :: Annex (Maybe UUID)
getUUID = getRepoUUID =<< gitRepo

{- Looks up a repo's UUID, caching it in .git/config if it's not already. -}
getRepoUUID :: Git.Repo -> Annex (Maybe UUID)
getRepoUUID r = do
	c <- toUUID <$> getConfig cachekey ""
	case getUncachedUUID r of
		v@(Just u)
			| c /= v -> do
				updatecache u
				return v
		_ -> return c
  where
	updatecache u = do
		g <- gitRepo
		when (g /= r) $ storeUUID cachekey u
	cachekey = remoteConfig r "uuid"

removeRepoUUID :: Annex ()
removeRepoUUID = unsetConfig configkey

getUncachedUUID :: Git.Repo -> Maybe UUID
getUncachedUUID = toUUID . Git.Config.get key ""
  where
	(ConfigKey key) = configkey

{- Make sure that the repo has an annex.uuid setting. -}
ensureUUID :: Annex UUID
ensureUUID = do
	mu <- getUUID
	case mu of
		Just u -> return u
		Nothing -> do
			u <- liftIO genUUID
			storeUUID configkey u
			return u

storeUUID :: ConfigKey -> UUID -> Annex ()
storeUUID configfield = setConfig configfield . fromUUID
