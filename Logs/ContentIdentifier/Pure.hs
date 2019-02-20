{- Remote content identifier logs, pure operations.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.ContentIdentifier.Pure 
	( ContentIdentifierLog
	, parseLog
	, buildLog
	) where

import Annex.Common
import Logs.MapLog
import Data.Int
import Types.Remote (ContentIdentifier(..))
import Utility.Base64

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

type ContentIdentifierLog = MapLog UUID [ContentIdentifier]

buildLog :: ContentIdentifierLog -> Builder
buildLog = buildMapLog buildUUID valuebuilder
  where
	valuebuilder [] = mempty
	valuebuilder [c] = buildcid c
	valuebuilder (c:cs) = buildcid c <> charUtf8 ' ' <> valuebuilder cs
	buildcid (ContentIdentifier c)
		| S8.any (`elem` [' ', '\r', '\n']) c || "!" `S8.isPrefixOf` c =
			charUtf8 '!' <> byteString (toB64' c)
		| otherwise = byteString c

parseLog :: L.ByteString -> ContentIdentifierLog
parseLog = parseMapLog
	(toUUID <$> A.takeByteString)
	(reverse . catMaybes <$> valueparser [])
  where
	valueparser l = do
		b <- A8.takeWhile1 (/= ' ')
		let cid = if "!" `S8.isPrefixOf` b
			then ContentIdentifier <$> fromB64Maybe' (S.drop 1 b)
			else Just $ ContentIdentifier b
		ifM A8.atEnd
			( return (cid:l)
			, do
				_ <- A8.char ' '
				valueparser (cid:l)
			)
