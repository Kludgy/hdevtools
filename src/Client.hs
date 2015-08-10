{-# LANGUAGE CPP #-}

module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Network ( connectTo
#ifdef mingw32_HOST_OS
               , PortID(PortNumber)
#else
               , PortID(UnixSocket)
#endif
               )
import System.Exit (exitFailure, exitWith)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

#ifndef mingw32_HOST_OS
import Daemonize (daemonize)
#endif
import Server (SocketDesc, createListenSocket, startServer)
import Types (ClientDirective(..), Command(..), ServerDirective(..))
import Util (readMaybe)

connect :: SocketDesc -> IO Handle
connect sock = do
#ifdef mingw32_HOST_OS
    connectTo "localhost" (PortNumber $ fromIntegral sock)
#else
    connectTo "" (UnixSocket sock)
#endif

getServerStatus :: SocketDesc -> IO ()
getServerStatus sock = do
    h <- connect sock
    hPutStrLn h $ show SrvStatus
    hFlush h
    startClientReadLoop h

stopServer :: SocketDesc -> IO ()
stopServer sock = do
    h <- connect sock
    hPutStrLn h $ show SrvExit
    hFlush h
    startClientReadLoop h

serverCommand :: SocketDesc -> Command -> [String] -> IO ()
serverCommand sock cmd ghcOpts = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            hPutStrLn h $ show (SrvCommand cmd ghcOpts)
            hFlush h
            startClientReadLoop h
#ifdef mingw32_HOST_OS
        Left _ -> error "no server"
#else
        Left _ -> do
            s <- createListenSocket sock
            daemonize False $ startServer sock (Just s)
            serverCommand sock cmd ghcOpts
#endif

startClientReadLoop :: Handle -> IO ()
startClientReadLoop h = do
    msg <- hGetLine h
    let clientDirective = readMaybe msg
    case clientDirective of
        Just (ClientStdout out) -> putStrLn out >> startClientReadLoop h
        Just (ClientStderr err) -> hPutStrLn stderr err >> startClientReadLoop h
        Just (ClientExit exitCode) -> hClose h >> exitWith exitCode
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err
        Nothing -> do
            hClose h
            unexpectedError $
                "The server sent an invalid message to the client: " ++ show msg

unexpectedError :: String -> IO ()
unexpectedError err = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    exitFailure
    where banner = replicate 78 '*'
