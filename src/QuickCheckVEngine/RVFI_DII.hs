--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018, 2020 Alexandre Joannou
-- Copyright (c) 2020 Alex Richardson
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- This software was partly developed by the University of Cambridge
-- Computer Laboratory as part of the Partially-Ordered Event-Triggered
-- Systems (POETS) project, funded by EPSRC grant EP/N031768/1.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
--

-- |
--    Module      : QuickCheckVEngine.RVFI_DII
--    Description : The RVFI-DII interface
--
--    This module re-exports the 'QuickCheckVEngine.RVFI_DII.RVFI' and
--    'QuickCheckVEngine.RVFI_DII.DII' modules, and provides functions to send and
--    receive 'DII_Packet's and 'RVFI_Packet's over a 'Socket'.
module QuickCheckVEngine.RVFI_DII
  ( module QuickCheckVEngine.RVFI_DII.RVFI,
    module QuickCheckVEngine.RVFI_DII.DII,
    sendDIIPacket,
    sendDIITrace,
    recvRVFITrace,
    rvfiNegotiateVersion,
  )
where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Int
import Network.Socket
import Network.Socket.ByteString.Lazy
import QuickCheckVEngine.RVFI_DII.DII
import QuickCheckVEngine.RVFI_DII.RVFI

-- | Send a single 'DII_Packet'
sendDIIPacket :: Socket -> DII_Packet -> IO ()
sendDIIPacket sckt inst = sendAll sckt $ BS.reverse (encode inst)

-- | Send an instruction trace (a '[DII_Packet]')
sendDIITrace :: (Socket, Int, String, Int) -> [DII_Packet] -> IO ()
sendDIITrace (sckt, _, _, _) trace = mapM_ (sendDIIPacket sckt) trace

-- | Receive a single 'RVFI_Packet'
recvRVFIPacket :: (Socket, Int) -> (String, Int) -> IO RVFI_Packet
recvRVFIPacket (sock, 1) (name, verbosity) = rvfiReadV1Response ((recvBlking sock), name, verbosity)
recvRVFIPacket (_, vers) (name, _) = error (name ++ " invalid trace version" ++ show vers)

-- | Receive an execution trace (a '[RVFI_Packet]')
recvRVFITrace :: (Socket, Int, String, Int) -> Bool -> IO [RVFI_Packet]
recvRVFITrace (sckt, traceVersion, name, verbosity) doLog = do
  rvfiPkt <- recvRVFIPacket (sckt, traceVersion) (name, verbosity)
  when doLog $ putStrLn $ "\t" ++ show rvfiPkt
  if rvfiIsHalt rvfiPkt
    then return [rvfiPkt]
    else do
      morePkts <- recvRVFITrace (sckt, traceVersion, name, verbosity) doLog
      return (rvfiPkt : morePkts)

-- | Perform a trace version negotiation with an implementation and return the
-- | accepted version.
rvfiNegotiateVersion :: Socket -> String -> Int -> IO Int
rvfiNegotiateVersion sckt name verbosity = do
  sendDIIPacket sckt diiVersNegotiate
  -- send a version negotiate packet, old implementations will return a halt
  -- packet with the halt field set to 1, newer implementations will use the
  -- high bits of that field to indicate their supported trace version
  rvfiPkt <- rvfiReadV1Response ((recvBlking sckt), name, verbosity)
  when (verbosity > 2) $
    putStrLn ("Received initial packet from " ++ name ++ ": " ++ show rvfiPkt)
  unless (rvfiIsHalt rvfiPkt) $
    error ("Received unexpected initial packet from " ++ name ++ ": " ++ show rvfiPkt)
  let supportedVer = rvfiHaltVersion rvfiPkt
  result <- diiSetVersion sckt (fromIntegral supportedVer) name verbosity
  when (result /= 2) $
    putStrLn ("WARNING: " ++ name ++ " does not support version 2 traces.")
  return result

-- | If supportedVersion > 1, send a 'v' command to set the trace version to v2
diiSetVersion :: Socket -> Int -> String -> Int -> IO Int
diiSetVersion sckt supportedVersion name verbosity = do
  if supportedVersion < 2
    then return 1
    else do
      let reqVersion = 2
      putStrLn ("Requesting version " ++ show reqVersion ++ " trace output from:" ++ name)
      sendDIIPacket sckt (diiRequestVers (fromIntegral reqVersion))
      -- Implementations should respond with a "version=" + UInt64 packet
      let connection = ((recvBlking sckt), name, verbosity)
      versionBytes <- rvfiReadDataPacketWithMagic connection 16 "version="
      let receivedVersion = fromIntegral (runGet Data.Binary.Get.getInt64le versionBytes)
      when (verbosity > 0) $
        putStrLn ("Received " ++ name ++ " set-version ack for version " ++ show receivedVersion)
      when (receivedVersion /= reqVersion) $
        putStrLn
          ( "WARNING: Received version response with unexpected version from " ++ name
              ++ ": got "
              ++ show receivedVersion
              ++ " but expected "
              ++ show reqVersion
          )
      when (receivedVersion > reqVersion) $
        error
          ( name ++ " uses a trace version " ++ show receivedVersion
              ++ " which is greater than the newest supported one!"
          )
      return receivedVersion

-- Internal helpers (not exported):
--------------------------------------------------------------------------------

-- | Receive a fixed number of bytes
recvBlking :: Socket -> Int64 -> IO BS.ByteString
recvBlking _ 0 = return BS.empty
recvBlking sckt n = do
  received <- Network.Socket.ByteString.Lazy.recv sckt n
  remainder <- recvBlking sckt (n - BS.length received)
  return $ BS.append received remainder
