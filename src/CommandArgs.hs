{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module CommandArgs
    ( HDevTools(..)
    , loadHDevTools
    )
where

import System.Console.CmdArgs.Implicit
import System.Environment (getProgName)
import System.Info (arch, os)
import qualified Config

#ifdef CABAL
import Data.Version (showVersion)
import Paths_hdevtools (version)
#endif

import Server (SocketDesc)

programVersion :: String
programVersion =
#ifdef CABAL
    "version " ++ showVersion version
#else
    "unknown-version (not built with cabal)"
#endif

fullVersion :: String
fullVersion =
    concat
        [ programVersion
        , " (ghc-", Config.cProjectVersion, "-", arch, "-", os, ")"
        ]

data HDevTools
    = Admin
        { socket :: Maybe SocketDesc
        , start_server :: Bool
        , noDaemon :: Bool
        , status :: Bool
        , stop_server :: Bool
        }
    | Check
        { socket :: Maybe SocketDesc
        , ghcOpts :: [String]
        , file :: String
        }
    | ModuleFile
        { socket :: Maybe SocketDesc
        , ghcOpts :: [String]
        , module_ :: String
        }
    | Info
        { socket :: Maybe SocketDesc
        , ghcOpts :: [String]
        , file :: String
        , identifier :: String
        }
    | Type
        { socket :: Maybe SocketDesc
        , ghcOpts :: [String]
        , file :: String
        , line :: Int
        , col :: Int
        }
    deriving (Show, Data, Typeable)

dummyAdmin :: HDevTools
dummyAdmin = Admin
    { socket = Nothing
    , start_server = False
    , noDaemon = False
    , status = False
    , stop_server = False
    }

dummyCheck :: HDevTools
dummyCheck = Check
    { socket = Nothing
    , ghcOpts = []
    , file = ""
    }

dummyModuleFile :: HDevTools
dummyModuleFile = ModuleFile
    { socket = Nothing
    , ghcOpts = []
    , module_ = ""
    }

dummyInfo :: HDevTools
dummyInfo = Info
    { socket = Nothing
    , ghcOpts = []
    , file = ""
    , identifier = ""
    }

dummyType :: HDevTools
dummyType = Type
    { socket = Nothing
    , ghcOpts = []
    , file = ""
    , line = 0
    , col = 0
    }

socketAnn :: Annotate Ann
socketAnn =
#ifdef mingw32_HOST_OS
    socket   := def += typ "PORT" += help "localhost port number to use"
#else
    socket   := def += typFile += help "socket file to use"
#endif

admin :: Annotate Ann
admin = record dummyAdmin
    [ socketAnn
    , start_server   := def            += help "start server"
    , noDaemon := def            += help "do not daemonize (only if --start-server)"
    , status   := def            += help "show status of server"
    , stop_server := def         += help "shutdown the server"
    ] += help "Interactions with the server"

check :: Annotate Ann
check = record dummyCheck
    [ socketAnn
    , ghcOpts  := def += typ "OPTION"   += help "ghc options"
    , file     := def += typFile      += argPos 0 += opt ""
    ] += help "Check a haskell source file for errors and warnings"

moduleFile :: Annotate Ann
moduleFile = record dummyModuleFile
    [ socketAnn
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , module_  := def += typ "MODULE" += argPos 0
    ] += help "Get the haskell source file corresponding to a module name"

info :: Annotate Ann
info = record dummyInfo
    [ socketAnn
    , ghcOpts    := def += typ "OPTION" += help "ghc options"
    , file       := def += typFile      += argPos 0 += opt ""
    , identifier := def += typ "IDENTIFIER" += argPos 1
    ] += help "Get info from GHC about the specified identifier"

type_ :: Annotate Ann
type_ = record dummyType
    [ socketAnn
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , file     := def += typFile      += argPos 0 += opt ""
    , line     := def += typ "LINE"   += argPos 1
    , col      := def += typ "COLUMN" += argPos 2
    ] += help "Get the type of the expression at the specified line and column"

full :: String -> Annotate Ann
full progName = modes_ [admin += auto, check, moduleFile, info, type_]
        += helpArg [name "h", groupname "Help"]
        += versionArg [groupname "Help"]
        += program progName
        += summary (progName ++ ": " ++ fullVersion)

loadHDevTools :: IO HDevTools
loadHDevTools = do
    progName <- getProgName
    (cmdArgs_ (full progName) :: IO HDevTools)
