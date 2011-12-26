{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TweakFetch where

import Common
import Command
import qualified Git.TweakFetch
import qualified Annex.Branch

def :: [Command]
def = [command "tweak-fetch" paramNothing seek "run by git tweak-fetch hook"]

seek :: [CommandSeek]
seek = [ withNothing start]

start :: CommandStart
start = do
	-- First, pass the hook's input through to its output, unchanged.
	fetched <- liftIO $ Git.TweakFetch.runHook return

	-- If one of the fetched refs is going to be stored on a git-annex
	-- tracking branch, then merge in the new sha for that ref.
	let tomerge = filter siblings fetched
	unless (null tomerge) $ Annex.Branch.updateTo $ map topairs tomerge
	stop
	where
		siblings f = suffix `isSuffixOf` (show $ Git.TweakFetch.local f)
		suffix = "/" ++ show Annex.Branch.name
		topairs f = (Git.TweakFetch.sha f, Git.TweakFetch.local f)
