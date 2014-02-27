{- xmpp client creds
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Creds where

import Assistant.Common
import Creds

import qualified Data.Text as T
import Network

xmppCredsFile :: FilePath
xmppCredsFile = "xmpp"

{- Everything we need to know to connect to an XMPP server. -}
data XMPPCreds = XMPPCreds
	{ xmppUsername :: T.Text
	, xmppPassword :: T.Text
	, xmppHostname :: HostName
	, xmppPort :: Int
	, xmppJID :: T.Text
	}
	deriving (Read, Show)

getXMPPCreds :: Annex (Maybe XMPPCreds)
getXMPPCreds = parse <$> readCacheCreds xmppCredsFile
  where
	parse s = readish =<< s

setXMPPCreds :: XMPPCreds -> Annex ()
setXMPPCreds creds = writeCacheCreds (show creds) xmppCredsFile
