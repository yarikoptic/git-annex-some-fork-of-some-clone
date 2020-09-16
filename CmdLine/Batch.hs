{- git-annex batch commands
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.Batch where

import Annex.Common
import Types.Command
import CmdLine.Action
import CmdLine.GitAnnex.Options
import CmdLine.Seek
import Options.Applicative
import Limit
import Types.FileMatcher
import Annex.BranchState
import Annex.WorkTree
import Annex.Content

data BatchMode = Batch BatchFormat | NoBatch

data BatchFormat = BatchLine | BatchNull

parseBatchOption :: Parser BatchMode
parseBatchOption = go 
	<$> switch
		( long "batch"
		<> help "enable batch mode"
		)
	<*> switch
		( short 'z'
		<> help "null delimited batch input"
		)
  where
	go True False = Batch BatchLine
 	go True True = Batch BatchNull
	go False _ = NoBatch

-- A batchable command can run in batch mode, or not.
-- In batch mode, one line at a time is read, parsed, and a reply output to
-- stdout. In non batch mode, the command's parameters are parsed and
-- a reply output for each.
--
-- Note that the actions are not run concurrently.
batchable :: (opts -> SeekInput -> String -> Annex Bool) -> Parser opts -> CmdParamsDesc -> CommandParser
batchable handler parser paramdesc = batchseeker <$> batchparser
  where
	batchparser = (,,)
		<$> parser
		<*> parseBatchOption
		<*> cmdParams paramdesc
	
	batchseeker (opts, NoBatch, params) =
		mapM_ (\p -> go NoBatch opts (SeekInput [p], p)) params
	batchseeker (opts, batchmode@(Batch fmt), _) = 
		batchInput fmt (pure . Right) (go batchmode opts)

	go batchmode opts (si, p) =
		unlessM (handler opts si p) $
			batchBadInput batchmode

-- bad input is indicated by an empty line in batch mode. In non batch
-- mode, exit on bad input.
batchBadInput :: BatchMode -> Annex ()
batchBadInput NoBatch = liftIO exitFailure
batchBadInput (Batch _) = liftIO $ putStrLn ""

-- Reads lines of batch mode input, runs a parser, and passes the result
-- to the action.
--
-- Note that if the batch input includes a worktree filename, it should
-- be converted to relative. Normally, filename parameters are passed
-- through git ls-files, which makes them relative, but batch mode does
-- not use that, and absolute worktree files are likely to cause breakage.
batchInput :: BatchFormat -> (String -> Annex (Either String v)) -> ((SeekInput, v) -> Annex ()) -> Annex ()
batchInput fmt parser a = go =<< batchLines fmt
  where
	go [] = return ()
	go (l:rest) = do
		either parseerr (\v -> a (SeekInput [l], v)) =<< parser l
		go rest
	parseerr s = giveup $ "Batch input parse failure: " ++ s

batchLines :: BatchFormat -> Annex [String]
batchLines fmt = do
	enableInteractiveBranchAccess
	liftIO $ splitter <$> getContents
  where
	splitter = case fmt of
		BatchLine -> lines
		BatchNull -> splitc '\0'

-- Runs a CommandStart in batch mode.
--
-- The batch mode user expects to read a line of output, and it's up to the
-- CommandStart to generate that output as it succeeds or fails to do its
-- job. However, if it stops without doing anything, it won't generate
-- any output, so in that case, batchBadInput is used to provide the caller
-- with an empty line.
batchCommandAction :: CommandStart -> Annex ()
batchCommandAction a = maybe (batchBadInput (Batch BatchLine)) (const noop)
	=<< callCommandAction' a

-- Reads lines of batch input and passes the filepaths to a CommandStart
-- to handle them.
--
-- Absolute filepaths are converted to relative, because in non-batch
-- mode, that is done when CmdLine.Seek uses git ls-files.
--
-- File matching options are checked, and non-matching files skipped.
batchFilesMatching :: BatchFormat -> ((SeekInput, RawFilePath) -> CommandStart) -> Annex ()
batchFilesMatching fmt a = do
	matcher <- getMatcher
	go $ \si f ->
		let f' = toRawFilePath f
		in ifM (matcher $ MatchingFile $ FileInfo f' f')
			( a (si, f')
			, return Nothing
			)
  where
	go a' = batchInput fmt 
		(Right <$$> liftIO . relPathCwdToFile)
		(batchCommandAction . uncurry a')

batchAnnexedFilesMatching :: BatchFormat -> AnnexedFileSeeker -> Annex ()
batchAnnexedFilesMatching fmt seeker = batchFilesMatching fmt $ \(si, bf) ->
	flip whenAnnexed bf $ \f k -> 
		case checkContentPresent seeker of
			Just v -> do
				present <- inAnnex k
				if present == v
					then startAction seeker si f k
					else return Nothing
			Nothing -> startAction seeker si f k
