{- git post-fetch hook support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.PostFetch (runHook, FetchedRef(..)) where

import Common
import Git
import Git.Types
import Git.Sha

{- Each line fed to the post-fetch hook should represent a ref that is
 - being updated. It's important that the hook always outputs every line
 - that is fed into it (possibly modified), otherwise incoming refs will
 - not be stored. So to avoid breaking if the format changes, unparsable
 - lines are stored as-is. -}
data FetchedRef = Unparsable String | FetchedRef
	{ sha :: Sha
	, merge :: Bool
	, remote :: Ref
	, local :: Ref
	}
	deriving (Show)

{- Runs the hook, allowing lines to be mutated and even produce more
 - than one output line, but never be discarded. Unparsable lines are
 - passed through unchanged. -}
runHook :: (FetchedRef -> IO [FetchedRef]) -> IO ()
runHook mutate = input >>= mapM callmutate >>= output . concat
	where
		callmutate u@(Unparsable _) = return [u]
		callmutate f = catchDefaultIO (mutate f) [f]

input :: IO [FetchedRef]
input = map parseLine . lines <$> getContents

output :: [FetchedRef] -> IO ()
output = mapM_ $ putStrLn . genLine

parseLine :: String -> FetchedRef
parseLine line = go $ words line
	where
		go [s, m, r, l]
			| not $ isSha s = Unparsable line
			| m == "merge" = parsed True
			| m == "not-for-merge" = parsed False
			| otherwise = Unparsable line
			where
				parsed v = FetchedRef
					{ sha = Ref s
					, merge = v
					, remote = Ref r
					, local = Ref l
					}
		go _ = Unparsable line

genLine :: FetchedRef -> String
genLine (Unparsable l) = l
genLine r = unwords
	[ show $ sha r
	, if merge r then "merge" else "not-for-merge"
	, show $ remote r
	, show $ local r
	]
