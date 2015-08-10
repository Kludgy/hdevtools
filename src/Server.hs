{-# LANGUAGE CPP #-}

module Server where

import Control.Exception (bracket, finally, handleJust, tryJust)
import Control.Monad (guard)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word16)
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import Network (
#ifdef mingw32_HOST_OS
                PortID(PortNumber)
#else
                PortID(UnixSocket)
#endif
                , Socket, accept, listenOn, sClose)
import System.Directory (removeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)

import CommandLoop (newCommandLoopState, startCommandLoop)
import Types (ClientDirective(..), Command, ServerDirective(..))
import Util (readMaybe)

#ifdef mingw32_HOST_OS
type SocketDesc = Word16     -- Word16 instead of PortNumber to support Data/Typeable
#else
type SocketDesc = UnixSocket
#endif


createListenSocket :: SocketDesc -> IO Socket
createListenSocket socketDesc =
#ifdef mingw32_HOST_OS
    listenOn (PortNumber $ fromIntegral socketDesc)
#else
    listenOn (UnixSocket socketDesc)
#endif

startServer :: SocketDesc -> Maybe Socket -> IO ()
startServer socketDesc mbSock = do
    case mbSock of
        Nothing -> bracket (createListenSocket socketDesc) cleanup go
        Just sock -> (go sock) `finally` (cleanup sock)
    where
    cleanup :: Socket -> IO ()
    cleanup sock = do
        sClose sock
#ifndef mingw32_HOST_OS
        removeSocketFile
#endif

    go :: Socket -> IO ()
    go sock = do
        state <- newCommandLoopState
        currentClient <- newIORef Nothing
        startCommandLoop state (clientSend currentClient) (getNextCommand currentClient sock) [] Nothing

#ifndef mingw32_HOST_OS
    removeSocketFile :: IO ()
    removeSocketFile = do
        -- Ignore possible error if socket file does not exist
        _ <- tryJust (guard . isDoesNotExistError) $ removeFile socketDesc
        return ()
#endif

clientSend :: IORef (Maybe Handle) -> ClientDirective -> IO ()
clientSend currentClient clientDirective = do
    mbH <- readIORef currentClient
    case mbH of
        Just h -> ignoreEPipe $ do
            hPutStrLn h (show clientDirective)
            hFlush h
        Nothing -> error "This is impossible"
    where
    -- EPIPE means that the client is no longer there.
    ignoreEPipe = handleJust (guard . isEPipe) (const $ return ())
    isEPipe = (==ResourceVanished) . ioeGetErrorType

getNextCommand :: IORef (Maybe Handle) -> Socket -> IO (Maybe (Command, [String]))
getNextCommand currentClient sock = do
    checkCurrent <- readIORef currentClient
    case checkCurrent of
        Just h -> hClose h
        Nothing -> return ()
    (h, _, _) <- accept sock
    writeIORef currentClient (Just h)
    msg <- hGetLine h -- TODO catch exception
    let serverDirective = readMaybe msg
    case serverDirective of
        Nothing -> do
            clientSend currentClient $ ClientUnexpectedError $
                "The client sent an invalid message to the server: " ++ show msg
            getNextCommand currentClient sock
        Just (SrvCommand cmd ghcOpts) -> do
            return $ Just (cmd, ghcOpts)
        Just SrvStatus -> do
            mapM_ (clientSend currentClient) $
                [ ClientStdout "Server is running."
                , ClientExit ExitSuccess
                ]
            getNextCommand currentClient sock
        Just SrvExit -> do
            mapM_ (clientSend currentClient) $
                [ ClientStdout "Shutting down server."
                , ClientExit ExitSuccess
                ]
            -- Must close the handle here because we are exiting the loop so it
            -- won't be closed in the code above
            hClose h
            return Nothing
