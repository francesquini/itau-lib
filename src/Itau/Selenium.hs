module Itau.Selenium
    ( seleniumRun
    ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.List
import GHC.IO.Handle
import Network.HTTP
import Network.URI (parseURI)
import System.Directory
import System.Environment.Executable
import System.Process
import Test.WebDriver
import Test.WebDriver.Firefox.Profile

myProfile :: String -> IO (PreparedProfile Firefox)
myProfile dir =        
        prepareProfile $ 
             addPref "browser.download.useDownloadDir"               True
           $ addPref "browser.download.dir"                          dir
           $ addPref "browser.download.folderList"                   (2 :: Integer)
           $ addPref "browser.download.manager.alertOnEXEOpen"       False
           $ addPref "browser.download.manager.closeWhenDone"        True
           $ addPref "browser.download.manager.focusWhenStarting"    False
           $ addPref "browser.download.manager.showAlertOnComplete"  False
           $ addPref "browser.download.manager.showWhenStarting"     False
           $ addPref "browser.download.manager.useWindow"            False
           $ addPref "browser.download.panel.shown"                  False
           $ addPref "browser.helperApps.alwaysAsk.force"            False
           -- sim, o Itau informa o content type como "content/type" :/
           $ addPref "browser.helperApps.neverAsk.saveToDisk"        ("content/type,text/plain" :: String)
           $ addPref "xpinstall.signatures.required"                 False
             defaultProfile

myConfig :: String -> IO WDConfig
myConfig dir = do
    pprof <- myProfile dir
    return $ defaultConfig {
        wdCapabilities = defaultCaps {
            browser = firefox {
                ffProfile = Just  pprof
            }
        }
    }

checkAndDownloadSelenium :: FilePath -> IO ()
checkAndDownloadSelenium fname = do
        fExists <- doesFileExist fname
        unless  fExists $ do
            putStrLn "Selenium server não encontrado. Iniciando download..."
            bytes <- simpleHTTP (defaultGETRequest_ url) >>= getResponseBody
            B.writeFile fname bytes
            putStrLn "Download terminado"
    where
        (Just url) = parseURI "http://selenium-release.storage.googleapis.com/2.46/selenium-server-standalone-2.46.0.jar"

seleniumRun :: String -> WD a -> IO ()
seleniumRun dir action = do
    seleniumStart
    config <- myConfig dir
    _ <- runSession config $ do
        setImplicitWait 100
        _ <- action
        closeSession
    seleniumStop
    return ()

seleniumStart :: IO ()
seleniumStart = do
        (path, _) <- splitExecutablePath
        let fname = fullFilePath path
        checkAndDownloadSelenium fname
        (_, _, Just st_err, _) <- createProcess (shell $ "java -jar " ++ fname){std_out = CreatePipe, std_err = CreatePipe}
        putStrLn "Iniciando Selenium Server"
        waitStart st_err
    where
        -- hardcoded, por enquanto, com a versão 2.46
        bname = "selenium-server-standalone-2.46.0.jar"
        fullFilePath path = path ++ bname
        waitStart fstream = do
            ln <- hGetLine fstream
            unless ("INFO - Selenium Server is up and running" `isSuffixOf` ln) $ waitStart fstream

seleniumStop :: IO ()
seleniumStop = do
        putStrLn "Desligando Selenium Server"
        res <- simpleHTTP (defaultGETRequest_ url) >>= getResponseBody ::IO String
        assert (res == "OKOK") $ return ()
    where
        (Just url) = parseURI "http://localhost:4444/selenium-server/driver/?cmd=shutDownSeleniumServer"
