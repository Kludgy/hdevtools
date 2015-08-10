{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ < 709
import Data.Traversable (traverse)
#endif

import Data.Maybe (fromMaybe)
import System.Directory (getCurrentDirectory)
import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), isAbsolute, takeDirectory)

import Cabal (findCabalFile)
import Client (getServerStatus, serverCommand, stopServer)
import CommandArgs
#ifndef mingw32_HOST_OS
import Daemonize (daemonize)
#endif
import Server (startServer, createListenSocket, SocketDesc)
import Types (Command(..), CommandExtra(..), emptyCommandExtra)

absoluteFilePath :: FilePath -> IO FilePath
absoluteFilePath p = if isAbsolute p then return p else do
    dir <- getCurrentDirectory
    return $ dir </> p


defaultSocketDesc :: SocketDesc
#ifdef mingw32_HOST_OS
defaultSocketDesc = 21001
#else
defaultSocketDesc = ".hdevtools.sock"
#endif


fileArg :: HDevTools -> Maybe String
fileArg (Admin {})      = Nothing
fileArg (ModuleFile {}) = Nothing
fileArg args@(Check {}) = Just $ file args
fileArg args@(Info  {}) = Just $ file args
fileArg args@(Type  {}) = Just $ file args

pathArg' :: HDevTools -> Maybe String
pathArg' (Admin {})      = Nothing
pathArg' (ModuleFile {}) = Nothing
pathArg' args@(Check {}) = path args
pathArg' args@(Info  {}) = path args
pathArg' args@(Type  {}) = path args

pathArg :: HDevTools -> Maybe String
pathArg args = case pathArg' args of
                Just x  -> Just x
                Nothing -> fileArg args

-- What happened to getSocketFilename?
getSocketDesc :: Maybe FilePath -> Maybe SocketDesc -> SocketDesc
#ifdef mingw32_HOST_OS

getSocketDesc _ Nothing = defaultSocketDesc
getSocketDesc _ (Just f) = f

#else

getSocketDesc mCabalFile mSocketDesc =
    let defaultSocketPath = maybe "" takeDirectory mCabalFile </> defaultSocketDesc in
    fromMaybe defaultSocketDesc mSocketDesc

#endif

main :: IO ()
main = do
    args <- loadHDevTools
    let argPath = pathArg args
    dir  <- maybe getCurrentDirectory (return . takeDirectory) argPath
    mCabalFile <- findCabalFile dir >>= traverse absoluteFilePath
    let extra = emptyCommandExtra
                    { ceGhcOptions  = ghcOpts args
                    , ceCabalConfig = mCabalFile
                    , cePath        = argPath
                    }
    let sock = getSocketDesc mCabalFile $ socket args
    case args of
        Admin {} -> doAdmin sock args extra
        Check {} -> doCheck sock args extra
        ModuleFile {} -> doModuleFile sock args extra
        Info {} -> doInfo sock args extra
        Type {} -> doType sock args extra

doAdmin :: SocketDesc -> HDevTools -> CommandExtra -> IO ()
doAdmin sock args _extra
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

doModuleFile :: SocketDesc -> HDevTools -> CommandExtra -> IO ()
doModuleFile sock args extra =
    serverCommand sock (CmdModuleFile (module_ args)) extra

doFileCommand :: String -> (HDevTools -> Command) -> SocketDesc -> HDevTools -> CommandExtra -> IO ()
doFileCommand cmdName cmd sock args extra
    | null (file args) = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a haskell source file. See:"
        hPutStrLn stderr $ progName ++ " " ++ cmdName ++ " --help"
    | otherwise = do
        absFile <- absoluteFilePath $ file args
        let args' = args { file = absFile }
        serverCommand sock (cmd args') extra

doCheck :: SocketDesc -> HDevTools -> CommandExtra -> IO ()
doCheck = doFileCommand "check" $
    \args -> CmdCheck (file args)

doInfo :: SocketDesc -> HDevTools -> CommandExtra -> IO ()
doInfo = doFileCommand "info" $
    \args -> CmdInfo (file args) (identifier args)

doType :: SocketDesc -> HDevTools -> CommandExtra -> IO ()
doType = doFileCommand "type" $
    \args -> CmdType (file args) (line args, col args)
