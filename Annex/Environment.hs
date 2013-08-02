{- git-annex environment
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Environment where

import Common.Annex
import Utility.Env
import Utility.UserInfo
import qualified Git.Config
import Config
import Annex.Exception

{- Checks that the system's environment allows git to function.
 - Git requires a GECOS username, or suitable git configuration, or
 - environment variables.
 -
 - Git also requires the system have a hostname containing a dot.
 - Otherwise, it tries various methods to find a FQDN, and will fail if it
 - does not. To avoid replicating that code here, which would break if its
 - methods change, this function does not check the hostname is valid.
 - Instead, code that commits can use ensureCommit.
 -}
checkEnvironment :: Annex ()
checkEnvironment = do
	gitusername <- fromRepo $ Git.Config.getMaybe "user.name"
	when (gitusername == Nothing || gitusername == Just "") $
		liftIO checkEnvironmentIO

checkEnvironmentIO :: IO ()
checkEnvironmentIO =
#ifdef mingw32_HOST_OS
	noop
#else
	whenM (null <$> myUserGecos) $ do
		username <- myUserName
		ensureEnv "GIT_AUTHOR_NAME" username
		ensureEnv "GIT_COMMITTER_NAME" username
  where
#ifndef __ANDROID__
  	-- existing environment is not overwritten
	ensureEnv var val = void $ setEnv var val False
#else
	-- Environment setting is broken on Android, so this is dealt with
	-- in runshell instead.
	ensureEnv _ _ = noop
#endif
#endif

{- Runs an action that commits to the repository, and if it fails, 
 - sets user.email to a dummy value and tries the action again. -}
ensureCommit :: Annex a -> Annex a
ensureCommit a = either retry return =<< tryAnnex a 
  where
  	retry _ = do
		setConfig (ConfigKey "user.email") =<< liftIO myUserName
		a
