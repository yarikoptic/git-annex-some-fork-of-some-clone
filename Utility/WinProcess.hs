{- Windows processes
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Utility.WinProcess where

import Utility.PID

#ifndef WITH_WINSPLICE
foreign import ccall unsafe "terminatepid"
	terminatePID :: PID -> IO ()
#else
terminatePID :: PID -> IO ()
terminatePID _ = return ()
#endif
