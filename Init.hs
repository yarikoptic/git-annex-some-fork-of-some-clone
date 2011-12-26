{- git-annex repository initialization
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Init (
	ensureInitialized,
	initialize,
	uninitialize
) where

import Common.Annex
import Utility.TempFile
import qualified Git
import qualified Annex.Branch
import Logs.UUID
import Annex.Version
import Annex.UUID

initialize :: Maybe String -> Annex ()
initialize mdescription = do
	prepUUID
	Annex.Branch.create
	setVersion
	gitHooksWrite
	u <- getUUID
	maybe (recordUUID u) (describeUUID u) mdescription

uninitialize :: Annex ()
uninitialize = gitHooksUnWrite

{- Will automatically initialize if there is already a git-annex
   branch from somewhere. Otherwise, require a manual init
   to avoid git-annex accidentially being run in git
   repos that did not intend to use it. -}
ensureInitialized :: Annex ()
ensureInitialized = getVersion >>= maybe needsinit checkVersion
	where
		needsinit = do
			annexed <- Annex.Branch.hasSibling
			if annexed
				then initialize Nothing
				else error "First run: git-annex init"

{- set up git hooks, if not already present -}
gitHooksWrite :: Annex ()
gitHooksWrite = unlessBare $ forM_ hooks $ \(hook, content) -> do
	file <- hookFile hook
	exists <- liftIO $ doesFileExist file
	if exists
		then warning $ hook ++ " hook (" ++ file ++ ") already exists, not configuring"
		else liftIO $ do
			viaTmp writeFile file content
			p <- getPermissions file
			setPermissions file $ p {executable = True}

gitHooksUnWrite :: Annex ()
gitHooksUnWrite = unlessBare $ forM_ hooks $ \(hook, content) -> do
	file <- hookFile hook
	whenM (liftIO $ doesFileExist file) $ do
		c <- liftIO $ readFile file
		if c == content
			then liftIO $ removeFile file
			else warning $ hook ++ " hook (" ++ file ++ 
				") contents modified; not deleting." ++
				" Edit it to remove call to git annex."

unlessBare :: Annex () -> Annex ()
unlessBare = unlessM $ fromRepo $ Git.repoIsLocalBare

hookFile :: FilePath -> Annex FilePath
hookFile f = (</>) <$> fromRepo Git.gitDir <*> pure ("hooks/" ++ f)

hooks :: [(String, String)]
hooks = [ ("pre-commit", hookscript "git annex pre-commit .")
	, ("tweak-fetch", hookscript "git annex tweak-fetch")
	]
	where
		hookscript s = "#!/bin/sh\n" ++
			"# automatically configured by git-annex\n" ++
			s ++ "\n";
