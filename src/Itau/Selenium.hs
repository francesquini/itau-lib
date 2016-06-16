module Itau.Selenium
    ( seleniumRun,
      Navegador (FF, CH),
      sleep,
      waitElemFound,
      clickAndWait,
      tryClickAndWait,
      tryClickAndWaitElem
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Types 
import qualified Data.ByteString as B
import           Data.List
import           Data.Text (pack)
import           GHC.IO.Handle
import           Network.HTTP
import           Network.URI (parseURI)
import           System.Directory
import           System.Environment.Executable
import           System.Process
import           Test.WebDriver
import           Test.WebDriver.Class
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Firefox.Profile
data Navegador = FF | CH

sleep :: Int -> IO()
sleep msecs =
    threadDelay $ msecs * 1000

waitElemFound :: (MonadIO wd, WebDriver wd) => Selector -> wd Element
waitElemFound sel =
    waitUntil 30 $ findElem sel

clickAndWait :: (MonadIO wd, WebDriver wd) => wd Element -> wd ()
clickAndWait el = do
    el >>= click
    liftIO $ sleep 3000
    return ()

tryClickAndWaitElem :: (MonadIO wd, WebDriver wd) => Selector -> wd ()
tryClickAndWaitElem el = tryClickAndWait $ findElem el

tryClickAndWait :: (MonadIO wd, WebDriver wd) => wd Element -> wd ()
tryClickAndWait el = waitUntil 30 $ clickAndWait el

myFFProfile :: String -> IO (PreparedProfile Firefox)
myFFProfile dir =        
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

myConfig :: Navegador -> String -> IO WDConfig
myConfig CH dir =
    return $ useBrowser chrome def
    where
        def = defaultConfig {
            wdCapabilities = defaultCaps {
                additionalCaps = [
                    ("download.default_directory", String $ pack dir), 
                    ("directory_upgrade", Bool True),
                    ("extensions_to_open", String $ pack "")
                    ]
                }
        }
myConfig FF dir = do
    pprof <- myFFProfile dir
    return $ defaultConfig {
        wdCapabilities = defaultCaps {
            additionalCaps = [("marionette", Bool True)],
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
        (Just url) = parseURI "http://selenium-release.storage.googleapis.com/2.53/selenium-server-standalone-2.53.0.jar"

seleniumRun :: Navegador -> String -> WD a -> IO ()
seleniumRun navegador dir action = do
    seleniumStart
    config <- myConfig navegador dir
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
        -- hardcoded, por enquanto, com a versão 2.53
        bname = "selenium-server-standalone-2.53.0.jar"
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
