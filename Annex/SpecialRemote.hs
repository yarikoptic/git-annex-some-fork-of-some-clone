{- git-annex special remote configuration
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.SpecialRemote where

import Annex.Common
import Remote (remoteTypes, remoteMap)
import Types.Remote (RemoteConfig, RemoteConfigKey, SetupStage(..), typename, setup)
import Types.GitConfig
import Logs.Remote
import Logs.Trust
import qualified Git.Config
import Git.Types (RemoteName)

import qualified Data.Map as M
import Data.Ord

newtype Sameas t = Sameas t

{- See if there's an existing special remote with this name.
 -
 - Prefer remotes that are not dead when a name appears multiple times. -}
findExisting :: RemoteName -> Annex (Maybe (UUID, RemoteConfig))
findExisting name = do
	t <- trustMap
	headMaybe
		. sortBy (comparing $ \(u, _c) -> Down $ M.lookup u t)
		. findByName name
		<$> Logs.Remote.readRemoteLog

newConfig :: RemoteName -> Maybe (Sameas UUID) -> RemoteConfig
newConfig name Nothing = M.singleton nameKey name
newConfig name (Just (Sameas u)) = M.fromList
	[ (sameasNameKey, name)
	, (sameasUUIDKey, fromUUID u)
	]

findByName :: RemoteName ->  M.Map UUID RemoteConfig -> [(UUID, RemoteConfig)]
findByName n = filter (matching . snd) . M.toList
  where
	matching c = case lookupName c of
		Nothing -> False
		Just n'
			| n' == n -> True
			| otherwise -> False

specialRemoteMap :: Annex (M.Map UUID RemoteName)
specialRemoteMap = do
	m <- Logs.Remote.readRemoteLog
	return $ M.fromList $ mapMaybe go (M.toList m)
  where
	go (u, c) = case lookupName c of
		Nothing -> Nothing
		Just n -> Just (u, n)

lookupName :: RemoteConfig -> Maybe RemoteName
lookupName c = M.lookup nameKey c <|> M.lookup sameasNameKey c

{- find the remote type -}
findType :: RemoteConfig -> Either String RemoteType
findType config = maybe unspecified specified $ M.lookup typeKey config
  where
	unspecified = Left "Specify the type of remote with type="
	specified s = case filter (findtype s) remoteTypes of
		[] -> Left $ "Unknown remote type " ++ s
		(t:_) -> Right t
	findtype s i = typename i == s

{- The name of a configured remote is stored in its config using this key. -}
nameKey :: RemoteConfigKey
nameKey = "name"

{- The name of a sameas remote is stored using this key instead. 
 - This prevents old versions of git-annex getting confused. -}
sameasNameKey :: RemoteConfigKey
sameasNameKey = "sameas-name"

{- The uuid that a sameas remote is the same as is stored in this key. -}
sameasUUIDKey :: RemoteConfigKey
sameasUUIDKey = "sameas-uuid"

{- The type of a remote is stored in its config using this key. -}
typeKey :: RemoteConfigKey
typeKey = "type"

autoEnableKey :: RemoteConfigKey
autoEnableKey = "autoenable"

autoEnable :: Annex ()
autoEnable = do
	remotemap <- M.filter configured <$> readRemoteLog
	enabled <- remoteMap id
	forM_ (M.toList remotemap) $ \(u, c) -> unless (u `M.member` enabled) $ do
		case (lookupName c, findType c) of
			(Just name, Right t) -> whenM (canenable u) $ do
				showSideAction $ "Auto enabling special remote " ++ name
				dummycfg <- liftIO dummyRemoteGitConfig
				tryNonAsync (setup t (Enable c) (Just u) Nothing c dummycfg) >>= \case
					Left e -> warning (show e)
					Right _ -> return ()
			_ -> return ()
  where
	configured rc = fromMaybe False $
		Git.Config.isTrue =<< M.lookup autoEnableKey rc
	canenable u = (/= DeadTrusted) <$> lookupTrust u
