{-# LANGUAGE CPP #-}

module Main where

import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)

import Client (getServerStatus, serverCommand, stopServer)
import CommandArgs
#ifndef mingw32_HOST_OS
import Daemonize (daemonize)
#endif
import Server (startServer, createListenSocket, SocketDesc)
import Types (Command(..))

defaultSocketDesc :: SocketDesc
#ifdef mingw32_HOST_OS
defaultSocketDesc = 21001
#else
defaultSocketDesc = ".hdevtools.sock"
#endif

getSocketDesc :: Maybe SocketDesc -> SocketDesc
getSocketDesc Nothing = defaultSocketDesc
getSocketDesc (Just f) = f

main :: IO ()
main = do
    args <- loadHDevTools
    let sock = getSocketDesc (socket args)
    case args of
        Admin {} -> doAdmin sock args
        Check {} -> doCheck sock args
        ModuleFile {} -> doModuleFile sock args
        Info {} -> doInfo sock args
        Type {} -> doType sock args

doAdmin :: SocketDesc -> HDevTools -> IO ()
doAdmin sock args
    | start_server args =
#ifdef mingw32_HOST_OS
        startServer sock Nothing
#else
        if noDaemon args then startServer sock Nothing
            else do
                s <- createListenSocket sock
                daemonize True $ startServer sock (Just s)
#endif
    | status args = getServerStatus sock
    | stop_server args = stopServer sock
    | otherwise = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a command. See:"
        hPutStrLn stderr $ progName ++ " --help"

doModuleFile :: SocketDesc -> HDevTools -> IO ()
doModuleFile sock args =
    serverCommand sock (CmdModuleFile (module_ args)) (ghcOpts args)

doFileCommand :: String -> (HDevTools -> Command) -> SocketDesc -> HDevTools -> IO ()
doFileCommand cmdName cmd sock args
    | null (file args) = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a haskell source file. See:"
        hPutStrLn stderr $ progName ++ " " ++ cmdName ++ " --help"
    | otherwise = serverCommand sock (cmd args) (ghcOpts args)

doCheck :: SocketDesc -> HDevTools -> IO ()
doCheck = doFileCommand "check" $
    \args -> CmdCheck (file args)

doInfo :: SocketDesc -> HDevTools -> IO ()
doInfo = doFileCommand "info" $
    \args -> CmdInfo (file args) (identifier args)

doType :: SocketDesc -> HDevTools -> IO ()
doType = doFileCommand "type" $
    \args -> CmdType (file args) (line args, col args)
