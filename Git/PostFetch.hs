{- git post-fetch hook support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.PostFetch where

import Common
import Git
import Git.Types
import Git.Sha

{- Each line fed to the post-fetch hook should represent a ref that is
 - being updated. To avoid breaking if the format changes, unparsable
 - lines are stored as-is. -}
data FetchedRef = Unparsable String | FetchedRef
	{ sha :: Sha
	, merge :: Bool
	, remote :: Ref
	, local :: Ref
	}
	deriving (Show)

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
