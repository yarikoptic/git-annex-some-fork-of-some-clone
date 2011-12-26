{- git tweak-fetch hook support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.TweakFetch (runHook, FetchedRef(..)) where

import Data.Either (rights)
import System.Posix.IO

import Common
import Git
import Git.Sha

data FetchedRef = FetchedRef
	{ sha :: Sha
	, merge :: Bool
	, remote :: Ref
	, local :: Ref
	}
	deriving (Show)

{- Each line fed to the tweak-fetch hook should represent a ref that is
 - being updated. It's important that the hook always outputs every line
 - that is fed into it (possibly modified), otherwise incoming refs will
 - not be stored. So to avoid breaking if the format changes, unparsable
 - lines are passed through unchanged. -}
type HookLine = Either String FetchedRef

{- Runs the hook, allowing lines to be mutated, but never be discarded.
 - Returns same FetchedRefs that are output by the hook, for further use. -}
runHook :: (FetchedRef -> IO FetchedRef) -> IO [FetchedRef]
runHook mutate = do
	ls <- mapM go =<< input
	output ls

	-- Nothing more should be output to stdout; only hook output
	-- is accepted by git. Redirect stdout to stderr.
	hFlush stdout
	_ <- liftIO $ dupTo stdError stdOutput

	return $ rights ls
	where
		go u@(Left _) = return u
		go (Right r) = Right <$> catchDefaultIO (mutate r) r

input :: IO [HookLine]
input = map parseLine . lines <$> getContents

output :: [HookLine] -> IO ()
output = mapM_ $ putStrLn . genLine

parseLine :: String -> HookLine
parseLine line = go $ words line
	where
		go [s, m, r, l]
			| not $ isSha s = Left line
			| m == "merge" = parsed True
			| m == "not-for-merge" = parsed False
			| otherwise = Left line
			where
				parsed v = Right $ FetchedRef
					{ sha = Ref s
					, merge = v
					, remote = Ref r
					, local = Ref l
					}
		go _ = Left line

genLine :: HookLine -> String
genLine (Left l) = l
genLine (Right r) = unwords
	[ show $ sha r
	, if merge r then "merge" else "not-for-merge"
	, show $ remote r
	, show $ local r
	]
