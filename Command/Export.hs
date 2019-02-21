{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TupleSections, BangPatterns #-}

module Command.Export where

import Command
import qualified Annex
import qualified Git
import qualified Git.DiffTree
import qualified Git.LsTree
import qualified Git.Ref
import Git.Types
import Git.FilePath
import Git.Sha
import Types.Remote
import Types.Export
import Annex.Export
import Annex.Content
import Annex.Transfer
import Annex.CatFile
import Annex.LockFile
import Logs.Location
import Logs.Export
import Database.Export
import Config
import Utility.Tmp
import Utility.Metered

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Concurrent

cmd :: Command
cmd = command "export" SectionCommon
	"export content to a remote"
	paramTreeish (seek <$$> optParser)

data ExportOptions = ExportOptions
	{ exportTreeish :: Git.Ref
	, exportRemote :: DeferredParse Remote
	, exportTracking :: Bool
	}

optParser :: CmdParamsDesc -> Parser ExportOptions
optParser _ = ExportOptions
	<$> (Git.Ref <$> parsetreeish)
	<*> (parseRemoteOption <$> parseToOption)
	<*> parsetracking
  where
	parsetreeish = argument str
		( metavar paramTreeish
		)
	parsetracking = switch
		( long "tracking"
		<> help ("track changes to the " ++ paramTreeish)
		)

-- To handle renames which swap files, the exported file is first renamed
-- to a stable temporary name based on the key.
exportTempName :: ExportKey -> ExportLocation
exportTempName ek = mkExportLocation $ 
	".git-annex-tmp-content-" ++ serializeKey (asKey (ek))

seek :: ExportOptions -> CommandSeek
seek o = do
	r <- getParsed (exportRemote o)
	unlessM (isExportSupported r) $
		giveup "That remote does not support exports."
	when (exportTracking o) $
		setConfig (remoteConfig r "export-tracking")
			(fromRef $ exportTreeish o)
	new <- fromMaybe (giveup "unknown tree") <$>
		-- Dereference the tree pointed to by the branch, commit,
		-- or tag.
		inRepo (Git.Ref.tree (exportTreeish o))
	withExclusiveLock (gitAnnexExportLock (uuid r)) $ do
		db <- openDb (uuid r)
		changeExport r db new
		unlessM (Annex.getState Annex.fast) $
			void $ fillExport r db new
		closeDb db

-- | Changes what's exported to the remote. Does not upload any new
-- files, but does delete and rename files already exported to the remote.
changeExport :: Remote -> ExportHandle -> Git.Ref -> CommandSeek
changeExport r db new = do
	old <- getExport (uuid r)
	recordExportBeginning (uuid r) new
	
	-- Clean up after incomplete export of a tree, in which
	-- the next block of code below may have renamed some files to
	-- temp files. Diff from the incomplete tree to the new tree,
	-- and delete any temp files that the new tree can't use.
	let recover diff = commandAction $
		startRecoverIncomplete r db
			(Git.DiffTree.srcsha diff)
			(Git.DiffTree.file diff)
	forM_ (incompleteExportedTreeishes old) $ \incomplete ->
		mapdiff recover incomplete new

	-- Diff the old and new trees, and delete or rename to new name all
	-- changed files in the export. After this, every file that remains
	-- in the export will have the content from the new treeish.
	-- 
	-- When there was an export conflict, this resolves it.
	--
	-- The ExportTree is also updated here to reflect the new tree.
	case exportedTreeishes old of
		[] -> updateExportTree db emptyTree new
		[oldtreesha] -> do
			diffmap <- mkDiffMap oldtreesha new db
			let seekdiffmap a = commandActions $ 
				map a (M.toList diffmap)
			-- Rename old files to temp, or delete.
			seekdiffmap $ \(ek, (moldf, mnewf)) -> do
				case (moldf, mnewf) of
					(Just oldf, Just _newf) ->
						startMoveToTempName r db oldf ek
					(Just oldf, Nothing) ->
						startUnexport' r db oldf ek
					_ -> stop
			-- Rename from temp to new files.
			seekdiffmap $ \(ek, (moldf, mnewf)) ->
				case (moldf, mnewf) of
					(Just _oldf, Just newf) ->
						startMoveFromTempName r db ek newf
					_ -> stop
		ts -> do
			warning "Export conflict detected. Different trees have been exported to the same special remote. Resolving.."
			forM_ ts $ \oldtreesha -> do
				-- Unexport both the srcsha and the dstsha,
				-- because the wrong content may have
				-- been renamed to the dstsha due to the
				-- export conflict.
				let unexportboth d = 
					[ Git.DiffTree.srcsha d 
					, Git.DiffTree.dstsha d
					]
				-- Don't rename to temp, because the
				-- content is unknown; delete instead.
				mapdiff
					(\diff -> commandAction $ startUnexport r db (Git.DiffTree.file diff) (unexportboth diff))
					oldtreesha new
			updateExportTree db emptyTree new
	liftIO $ recordExportTreeCurrent db new

	-- Waiting until now to record the export guarantees that,
	-- if this export is interrupted, there are no files left over
	-- from a previous export, that are not part of this export.
	c <- Annex.getState Annex.errcounter
	when (c == 0) $ do
		recordExport (uuid r) $ ExportChange
			{ oldTreeish = exportedTreeishes old
			, newTreeish = new
			}
  where
	mapdiff a oldtreesha newtreesha = do
		(diff, cleanup) <- inRepo $
			Git.DiffTree.diffTreeRecursive oldtreesha newtreesha
		seekActions $ pure $ map a diff
		void $ liftIO cleanup

-- Map of old and new filenames for each changed ExportKey in a diff.
type DiffMap = M.Map ExportKey (Maybe TopFilePath, Maybe TopFilePath)

mkDiffMap :: Git.Ref -> Git.Ref -> ExportHandle -> Annex DiffMap
mkDiffMap old new db = do
	(diff, cleanup) <- inRepo $ Git.DiffTree.diffTreeRecursive old new
	diffmap <- M.fromListWith combinedm . concat <$> forM diff mkdm
	void $ liftIO cleanup
	return diffmap
  where
	combinedm (srca, dsta) (srcb, dstb) = (srca <|> srcb, dsta <|> dstb)
	mkdm i = do
		srcek <- getek (Git.DiffTree.srcsha i)
		dstek <- getek (Git.DiffTree.dstsha i)
		updateExportTree' db srcek dstek i
		return $ catMaybes
			[ (, (Just (Git.DiffTree.file i), Nothing)) <$> srcek
			, (, (Nothing, Just (Git.DiffTree.file i))) <$> dstek
			]
	getek sha
		| sha == nullSha = return Nothing
		| otherwise = Just <$> exportKey sha

-- | Upload all exported files that are not yet in the remote,
-- Returns True when files were uploaded.
fillExport :: Remote -> ExportHandle -> Git.Ref -> Annex Bool
fillExport r db new = do
	(l, cleanup) <- inRepo $ Git.LsTree.lsTree Git.LsTree.LsTreeRecursive new
	cvar <- liftIO $ newMVar False
	commandActions $ map (startExport r db cvar) l
	void $ liftIO $ cleanup
	liftIO $ takeMVar cvar

startExport :: Remote -> ExportHandle -> MVar Bool -> Git.LsTree.TreeItem -> CommandStart
startExport r db cvar ti = do
	ek <- exportKey (Git.LsTree.sha ti)
	stopUnless (notrecordedpresent ek) $ do
		showStart ("export " ++ name r) f
		ifM (either (const False) id <$> tryNonAsync (checkPresentExport (exportActions r) (asKey ek) loc))
			( next $ next $ cleanupExport r db ek loc False
			, do
				liftIO $ modifyMVar_ cvar (pure . const True)
				next $ performExport r db ek af (Git.LsTree.sha ti) loc
			)
  where
	loc = mkExportLocation f
	f = getTopFilePath (Git.LsTree.file ti)
	af = AssociatedFile (Just f)
	notrecordedpresent ek = (||)
		<$> liftIO (notElem loc <$> getExportedLocation db (asKey ek))
		-- If content was removed from the remote, the export db
		-- will still list it, so also check location tracking.
		<*> (notElem (uuid r) <$> loggedLocations (asKey ek))

performExport :: Remote -> ExportHandle -> ExportKey -> AssociatedFile -> Sha -> ExportLocation -> CommandPerform
performExport r db ek af contentsha loc = do
	let storer = storeExport (exportActions r)
	sent <- case ek of
		AnnexKey k -> ifM (inAnnex k)
			( notifyTransfer Upload af $
				-- Using noRetry here because interrupted
				-- exports cannot be resumed.
				upload (uuid r) k af noRetry $ \pm -> do
					let rollback = void $
						performUnexport r db [ek] loc
					sendAnnex k rollback $ \f ->
						storer f k loc pm
			, do
				showNote "not available"
				return False
			)
		-- Sending a non-annexed file.
		GitKey sha1k ->
			withTmpFile "export" $ \tmp h -> do
				b <- catObject contentsha
				liftIO $ L.hPut h b
				liftIO $ hClose h
				storer tmp sha1k loc nullMeterUpdate
	if sent
		then next $ cleanupExport r db ek loc True
		else stop

cleanupExport :: Remote -> ExportHandle -> ExportKey -> ExportLocation -> Bool -> CommandCleanup
cleanupExport r db ek loc sent = do
	liftIO $ addExportedLocation db (asKey ek) loc
	when sent $
		logChange (asKey ek) (uuid r) InfoPresent
	return True

startUnexport :: Remote -> ExportHandle -> TopFilePath -> [Git.Sha] -> CommandStart
startUnexport r db f shas = do
	eks <- forM (filter (/= nullSha) shas) exportKey
	if null eks
		then stop
		else do
			showStart ("unexport " ++ name r) f'
			next $ performUnexport r db eks loc
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f

startUnexport' :: Remote -> ExportHandle -> TopFilePath -> ExportKey -> CommandStart
startUnexport' r db f ek = do
	showStart ("unexport " ++ name r) f'
	next $ performUnexport r db [ek] loc
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f

-- Unlike a usual drop from a repository, this does not check that
-- numcopies is satisfied before removing the content. Typically an export
-- remote is untrusted, so would not count as a copy anyway.
-- Or, an export may be appendonly, and removing a file from it does
-- not really remove the content, which must be accessible later on.
performUnexport :: Remote -> ExportHandle -> [ExportKey] -> ExportLocation -> CommandPerform
performUnexport r db eks loc = do
	ifM (allM (\ek -> removeExport (exportActions r) (asKey ek) loc) eks)
		( next $ cleanupUnexport r db eks loc
		, stop
		)

cleanupUnexport :: Remote -> ExportHandle -> [ExportKey] -> ExportLocation -> CommandCleanup
cleanupUnexport r db eks loc = do
	liftIO $ do
		forM_ eks $ \ek ->
			removeExportedLocation db (asKey ek) loc
		flushDbQueue db

	-- An appendonly remote can support removeExportLocation to remove
	-- the file from the exported tree, but still retain the content
	-- and allow retrieving it.
	unless (appendonly r) $ do
		remaininglocs <- liftIO $ 
			concat <$> forM eks (\ek -> getExportedLocation db (asKey ek))
		when (null remaininglocs) $
			forM_ eks $ \ek ->
				logChange (asKey ek) (uuid r) InfoMissing
	
	removeEmptyDirectories r db loc (map asKey eks)

startRecoverIncomplete :: Remote -> ExportHandle -> Git.Sha -> TopFilePath -> CommandStart
startRecoverIncomplete r db sha oldf
	| sha == nullSha = stop
	| otherwise = do
		ek <- exportKey sha
		let loc = exportTempName ek
		showStart ("unexport " ++ name r) (fromExportLocation loc)
		liftIO $ removeExportedLocation db (asKey ek) oldloc
		next $ performUnexport r db [ek] loc
  where
	oldloc = mkExportLocation oldf'
	oldf' = getTopFilePath oldf

startMoveToTempName :: Remote -> ExportHandle -> TopFilePath -> ExportKey -> CommandStart
startMoveToTempName r db f ek = do
	showStart ("rename " ++ name r) (f' ++ " -> " ++ fromExportLocation tmploc)
	next $ performRename r db ek loc tmploc
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f
	tmploc = exportTempName ek

startMoveFromTempName :: Remote -> ExportHandle -> ExportKey -> TopFilePath -> CommandStart
startMoveFromTempName r db ek f = do
	let tmploc = exportTempName ek
	stopUnless (liftIO $ elem tmploc <$> getExportedLocation db (asKey ek)) $ do
		showStart ("rename " ++ name r) (fromExportLocation tmploc ++ " -> " ++ f')
		next $ performRename r db ek tmploc loc
  where
	loc = mkExportLocation f'
	f' = getTopFilePath f

performRename :: Remote -> ExportHandle -> ExportKey -> ExportLocation -> ExportLocation -> CommandPerform
performRename r db ek src dest = do
	ifM (renameExport (exportActions r) (asKey ek) src dest)
		( next $ cleanupRename r db ek src dest
		-- In case the special remote does not support renaming,
		-- unexport the src instead.
		, do
			warning "rename failed; deleting instead"
			performUnexport r db [ek] src
		)

cleanupRename :: Remote -> ExportHandle -> ExportKey -> ExportLocation -> ExportLocation -> CommandCleanup
cleanupRename r db ek src dest = do
	liftIO $ do
		removeExportedLocation db (asKey ek) src
		addExportedLocation db (asKey ek) dest
		flushDbQueue db
	if exportDirectories src /= exportDirectories dest
		then removeEmptyDirectories r db src [asKey ek]
		else return True

-- | Remove empty directories from the export. Call after removing an
-- exported file, and after calling removeExportLocation and flushing the
-- database.
removeEmptyDirectories :: Remote -> ExportHandle -> ExportLocation -> [Key] -> Annex Bool
removeEmptyDirectories r db loc ks
	| null (exportDirectories loc) = return True
	| otherwise = case removeExportDirectory (exportActions r) of
		Nothing -> return True
		Just removeexportdirectory -> do
			ok <- allM (go removeexportdirectory) 
				(reverse (exportDirectories loc))
			unless ok $ liftIO $ do
				-- Add location back to export database, 
				-- so this is tried again next time.
				forM_ ks $ \k ->
					addExportedLocation db k loc
				flushDbQueue db
			return ok
  where
	go removeexportdirectory d = 
		ifM (liftIO $ isExportDirectoryEmpty db d)
			( removeexportdirectory d
			, return True
			)
