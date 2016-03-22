module Itau
    (
      getUserParams
    , itauRun
    , itauRunDir
    , login
    , logout
    , getCSV
    , getOFX
    , ItauAccountInfo
    , getAccountInfo
    , getCCInfo
    , itauCardInfoToCSV
    )
     where

import           Control.Monad.IO.Class
import           Data.List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Text                    (Text, pack, toUpper)
import qualified Data.Text                    as T
import           Data.Time.Calendar
import           Itau.Utils
import           Itau.Selenium
import           Test.WebDriver
import qualified Test.WebDriver.Class         as WD
import           Test.WebDriver.Commands.Wait
import           Text.Printf
import           Text.Regex
import           System.Directory

import           Debug.Trace

data ItauUserParams = ItauUserParams
    String -- agencia
    String -- conta
    String -- nome
    String -- senha
    deriving Show

data ItauAccountInfo = ItauAccountInfo {
      accSaldoProvisorio :: Float
    , accSaldoDisponivel :: Float
    , accSaldoBloqueado  :: Float
    , accCEP             :: Float
    , accCEPPlus         :: Float
    , accTotalComLimite  :: Float
    , accSaldoEfetivo    :: Float
    , accJurosDias       :: Int
    , accJurosValor      :: Float
    , accJurosVencimento :: Day
    , accJurosTaxaMensal :: Float
    , accJurosTaxaAnual  :: Float
    , accJurosCETMensal  :: Float
    , accJurosCETAnual   :: Float
    , accFutureTx        :: [ItauTransaction]
    } deriving Show

data ItauTransaction = ItauTransaction {
      txData      :: Day
    , txDescricao :: Text
    , txValor     :: Float
    , txOrigem    :: Text
    , txFlag      :: Text
    } deriving Show

data ItauCardFatura = ItauCardFatura {
      ccfAberta     :: Bool
    , ccfVencimento :: Day
    , ccfValorTotal :: Float
    , ccfTxs        :: [ItauTransaction]
    } deriving Show

data ItauCardInfo = ItauCardInfo {
      ccProximaFatura  :: ItauCardFatura
    , ccFaturaAtual    :: ItauCardFatura
    , ccFaturaAnterior :: ItauCardFatura
    } deriving Show

type Cell = Text
type Table = [[Cell]]


getUserParams :: IO ItauUserParams
getUserParams = do
    agencia <- getUserInput "Agência: " True
    conta   <- getUserInput "Conta corrente: " True
    nome    <- getUserInput "Primeiro nome: " True
    senha   <- getUserInput "Senha: " False
    return $ ItauUserParams agencia conta nome senha
    
itauRun :: WD a -> IO ()
itauRun action = do 
    dir <- getCurrentDirectory 
    itauRunDir dir action

itauRunDir :: String -> WD a -> IO ()
itauRunDir = seleniumRun

login :: ItauUserParams -> WD()
login (ItauUserParams agencia conta nome senha) = do
    openPage "http://www.itau.com.br"
    -- Primeira etapa, digita ag/cc e pede acesso
    findElem (ById "campo_agencia") >>= sendKeys (pack agencia)
    findElem (ById "campo_conta")   >>= sendKeys (pack conta)
    findElem (ByLinkText "Acessar") >>= click
    -- Seleciona o usuário
    _ <- waitUntil 30 $ findElem (ByLinkText $ toUpper (pack nome)) >>= click
    liftIO $ sleep 500
    -- Digita a senha
    allButtons <- findPasswordButtons senha
    let bs = map (\s -> fromJust (Map.lookup s allButtons)) senha
    _ <- mapM click bs
    liftIO $ sleep 500
    -- Completa o login
    findElem (ByCSS "img[alt=\"Continuar\"]") >>= click

logout :: WD()
logout = do
    findElem (ByCSS "img.btnSair") >>= click
    findElem (ByCSS "img[alt=\"Sair\"]") >>= click
    liftIO $ sleep 5000 -- Aguarda alguns segundos para ter certeza que o logout terminou

getOFX :: WD()
getOFX = downloadFileType "OFX"

getCSV :: WD()
getCSV = downloadFileType "TXT"

getAccountInfo :: WD (Either Text ItauAccountInfo)
getAccountInfo = do
    goToExtrato
    tableLines <- findElems (ByXPath "//div[@id='TRNcontainer01']/div[@id='ExtratoX0']/table[1]//tr")
    tableCells <- mapM (`findElemsFrom` ByTag "td") tableLines
    table <- mapM (mapM getText) tableCells :: WD Table
    day <- liftIO today
    return $ buildAccountInfo day table

getCCInfo :: WD (Either Text ItauCardInfo)
getCCInfo = do
    -- Se a data for antes do vencimento do cartão a fechada é selecionada, senão a prox é selecionada
    -- Como a anterior nunca é selecionada, começamos por ela e não precisamos descobrir onde estamos
    goToCartao
    fatAnt   <- goToFaturaAnterior >> scrapAndBuildCCFatura False
    fatAtual <- goToFaturaAtual >> scrapAndBuildCCFatura False
    fatProx  <- goToFaturaProxima >> scrapAndBuildCCFatura True
    return $ ccInfoSmokeTest (emptyItauCardInfo {
          ccProximaFatura  = fatProx
        , ccFaturaAtual    = fatAtual
        , ccFaturaAnterior = fatAnt
        })

itauCardInfoToCSV :: ItauCardInfo -> Text
itauCardInfoToCSV info =
    T.unlines $ map txToCSV txs
    where
        txs = classificaCCParcelamento $ concat [ccfTxs $ ccProximaFatura info, ccfTxs $ ccFaturaAtual info, ccfTxs $ ccFaturaAnterior info]

---------------------
-- LOCAL FUNCTIONS --
---------------------

---------------------
-- Login
---------------------

findPasswordButtons :: String -> WD (Map Char Element)
findPasswordButtons passwd =
    findPasswordButtons' digits Map.empty
    where
        digits = sort $ nub passwd

findPasswordButtons' :: WD.WebDriver m => String -> Map Char Element -> m (Map Char Element)
findPasswordButtons' [] found = return found
findPasswordButtons' (d:ds) found = do
        elems <- mapM fElems (filter (elem d) possibleButtons)
        let butt = head $ concat elems
        findPasswordButtons' ds (Map.insert d butt found)
    where
        fElems x = findElems $ ByCSS (pack $ "img[title=\"" ++ x ++ "\"]")



possibleButtons :: [String]
possibleButtons =
        map inter $ comb 2 ['0'..'9']
    where
        inter :: String -> String
        inter x = intercalate " ou " (map (:[]) x)


---------------------
-- Navigation
---------------------

goToExtrato :: WD ()
goToExtrato = do
    waitUntil 30 $ findElem (ByLinkText "Conta Corrente") >>= click
    waitUntil 30 $ findElem (ByLinkText "Extrato") >>= click
    _ <- waitUntil 30 $ findElem (ById "ExtratoX0")
    return ()


goToCartao :: WD ()
goToCartao = do
    findElem (ByLinkText "Cartões") >>= click
    _ <- waitUntil 30 $ findElem (ByLinkText "Ver fatura e limites") >>= click
    _ <- waitUntil 30 $ findElem (ByCSS "img[alt=\"Fatura anterior\"]")
    return ()

--precisa ja estar dentro do cartao e nao pode estar na fatura atual
goToFaturaAtual :: WD ()
goToFaturaAtual = do
    findElem (ByCSS "img[alt=\"Fatura atual\"]") >>= click
    _ <- waitUntil 30 $ findElem (ByCSS "img[alt=\"Fatura anterior\"]") -- o link clicado some, checa outro item
    return ()

--precisa ja estar dentro do cartao e nao pode estar na fatura anterior
goToFaturaAnterior :: WD ()
goToFaturaAnterior = do
    findElem (ByCSS "img[alt=\"Fatura anterior\"]") >>= click
    _ <- waitUntil 30 $ findElem (ByCSS "img[alt=\"Fatura atual\"]") -- o link clicado some, checa outro item
    return ()

goToFaturaProxima :: WD ()
goToFaturaProxima = do
    findElem (ByCSS "img[alt=\"Próxima fatura\"]") >>= click
    _ <- waitUntil 30 $ findElem (ByCSS "img[alt=\"Fatura atual\"]") -- o link clicado some, checa outro item
    return ()

---------------------
-- OFX/CSV
---------------------

getFileDownloadStartDate :: IO (Text, Text, Text)
getFileDownloadStartDate = do
    twoMonthsAgo <- addDays (-60) <$> today  -- Limite estabelecido pelo Itau
    return . toText $ toGregorian twoMonthsAgo
    where
        fmt2 = pack . printf "%02d"
        toText (year, month, day) = (pack $ show year, fmt2 month, fmt2 day)

downloadFileType :: Text -> WD ()
downloadFileType fType = do
    goToExtrato
    waitUntil 30 $ findElem (ByLinkText "Salvar em outros formatos") >>= click
    (year, month, day) <- liftIO getFileDownloadStartDate
    liftIO $ sleep 5000 -- Apesar da página completa, um script passa zerando os valores dos campos se formos muito rápido
    waitUntil 30 $ findElem (ById "Dia") >>= sendKeys day
    liftIO $ sleep 500
    waitUntil 30 $ findElem (ById "Mes") >>= sendKeys month
    liftIO $ sleep 500
    waitUntil 30 $ findElem (ById "Ano") >>= sendKeys year
    liftIO $ sleep 500
    waitUntil 30 $ findElem (ByCSS (T.concat ["input[value=\"", fType, "\"]"])) >>= click
    waitUntil 30 $ findElem (ByCSS "img.TRNinputBTN") >>= click
    liftIO $ sleep 5000 -- aguarda alguns segundos pra que o download tenha acabado

---------------------
-- ACCOUNT
---------------------

buildAccountInfo :: Day -> Table -> Either Text ItauAccountInfo
buildAccountInfo day tableCells =
    accInfoSmokeTest $
        matchAndFillFutureTxs day futTx $
        matchAndFillCEP day cep $
        matchAndFillGenInfo genInfo
        emptyItauAccountInfo
    where
        genInfo = dropTake 2 4 "Posição da Conta Corrente"
        cep     = dropTake 2 3 "CEP/CEP Plus"
        futTx   = dropTake 9 3 "Lançamentos futuros"
        dropTake len drops header =
            takeWhile (\a -> len == length a) $ drop drops $ dropWhile (/= [header]) tableCells

accInfoSmokeTest :: ItauAccountInfo -> Either Text ItauAccountInfo
accInfoSmokeTest info =
    case [ msg | (msg, tst) <- tests, not tst] of
        []   -> Right info
        msgs -> Left $ pack $ "Informações da conta inconsistentes. " ++ intercalate ", " msgs
    where
        jurosAnuaisDadoMensal i = ((accJurosTaxaMensal i / 100 + 1) ** 12 - 1) * 100
        tests = [
                  ("Saldos Provisório + Bloqueado != Disponível ("
                    ++ show (accSaldoProvisorio info) ++ "  " ++ show (accSaldoBloqueado info) ++ ")",
                   (accSaldoProvisorio info + accSaldoBloqueado info) `fracEquals2` accSaldoDisponivel info)
                , ("CEP + CEP Plus + Saldo Provisório + Bloqueado != Total Disponível com limite ("
                    ++ show (accCEP info) ++ " " ++ show (accCEPPlus info) ++ " "
                    ++ show (accSaldoBloqueado info) ++ " " ++ show (accSaldoProvisorio info) ++ ")",
                   (accCEP info + accCEPPlus info + accSaldoProvisorio info + accSaldoBloqueado info) `fracEquals2` accTotalComLimite info)
                , ("Juros não estão entre 6 e 15%: (" ++ show (accJurosTaxaMensal info) ++ ")",
                   (accJurosTaxaMensal info > 6) && (accJurosTaxaMensal info < 15))
                , ("Juros anuais obtidos tem que ser próximo ao calculado com a base mensal (" ++ show (jurosAnuaisDadoMensal info) ++ ")",
                   jurosAnuaisDadoMensal info `fracEquals2` accJurosTaxaAnual info)
                , ("Juros Mensais + IOF != CET Mensal (Mensal: " ++ show (accJurosTaxaMensal info * 1.0683) ++ " CET " ++ show (accJurosCETMensal info) ++ ")",
                   ((accJurosTaxaMensal info * 1.075) - accJurosCETMensal info) < 0.5)
                ]

emptyItauAccountInfo :: ItauAccountInfo
emptyItauAccountInfo =
    ItauAccountInfo {
        accSaldoProvisorio = 0
      , accSaldoBloqueado  = 0
      , accSaldoDisponivel = 0
      , accCEP             = 0
      , accCEPPlus         = 0
      , accTotalComLimite  = 0
      , accSaldoEfetivo    = 0
      , accJurosDias       = 0
      , accJurosValor      = 0
      , accJurosVencimento = fromGregorian 1900 1 1
      , accJurosTaxaMensal = 0
      , accJurosTaxaAnual  = 0
      , accJurosCETMensal  = 0
      , accJurosCETAnual   = 0
      , accFutureTx        = []
      }

matchAndFillGenInfo :: Table -> ItauAccountInfo -> ItauAccountInfo
matchAndFillGenInfo ls accInfo =
        foldl matchAndFillGenInfo' accInfo ls
    where
        matchAndFillGenInfo' info ["(+) SALDO PROVISORIO CONTA", val] =
            info {accSaldoProvisorio = fromItauNum val}
        matchAndFillGenInfo' info ["(-) CRED DIA A COMPENSAR (C)", val] =
            info {accSaldoBloqueado  = accSaldoBloqueado info + fromItauNum val}
        matchAndFillGenInfo' info ["(-) CREDITOS NAO LIBERADOS", val] =
            info {accSaldoBloqueado  = accSaldoBloqueado info + fromItauNum val}
        matchAndFillGenInfo' info ["(=) SALDO DISPONIVEL PARA SAQUE", val] =
            info {accSaldoDisponivel = fromItauNum val}
        matchAndFillGenInfo' info ["(+) CEP(1) 10 DIAS S/ JUROS", val] = 
            info {accCEP             = accCEP info + fromItauNum val}
        matchAndFillGenInfo' info ["(+) CEP (SUJEITO A ENCARGOS)", val] =
            info {accCEP             = accCEP info + fromItauNum val}
        matchAndFillGenInfo' info ["(+) CEP PLUS(1) 10 DIAS S/ JUROS", val] =
            info {accCEPPlus         = accCEPPlus info + fromItauNum val}
        matchAndFillGenInfo' info ["(+) LIM ADIC CEP PLUS (SUJ. ENCARGOS)", val] =
            info {accCEPPlus         = accCEPPlus info + fromItauNum val}
        matchAndFillGenInfo' info ["(=) VALOR TOTAL DISPONIVEL PARA SAQUE", val] =
            info {accTotalComLimite  = fromItauNum val}
        matchAndFillGenInfo' info ["(=) SDO DISP P/ APLIC HOJE", val] =
            info {accSaldoEfetivo    = fromItauNum val}
        matchAndFillGenInfo' info ~(txt:_) =
            trace ("Desconhecido" ++ T.unpack txt) info
        
matchAndFillCEP :: Day -> Table -> ItauAccountInfo -> ItauAccountInfo
matchAndFillCEP day ls accInfo =
        foldl matchAndFillCEP' accInfo ls
    where
        matchAndFillCEP' info ["QTDE. DIAS UTILIZADOS NO PERÍODO", val] = info {accJurosDias = truncate (fromItauNum val)}
        matchAndFillCEP' info ["JUROS DO LIMITE", val]                  = info {accJurosValor = fromItauNum val}
        matchAndFillCEP' info [" DATA DE VENCIMENTO", val]              = info {accJurosVencimento = fromItauDate day Forward val}
        matchAndFillCEP' info [" TAXA DE JUROS MENSAL", val]            = info {accJurosTaxaMensal = fromItauNum val}
        matchAndFillCEP' info [" TAXA DE JUROS ANUAL", val]             = info {accJurosTaxaAnual = fromItauNum val}
        matchAndFillCEP' info [" CUSTO EFETIVO TOTAL(CET) AO MÊS", val] = info {accJurosCETMensal = fromItauNum val}
        matchAndFillCEP' info [" CUSTO EFETIVO TOTAL(CET) AO ANO", val] = info {accJurosCETAnual = fromItauNum val}
        matchAndFillCEP' info ~(txt:_) = trace ("Desconhecido" ++ T.unpack txt) info

matchAndFillFutureTxs :: Day -> Table -> ItauAccountInfo -> ItauAccountInfo
matchAndFillFutureTxs day ls accInfo =
        accInfo {accFutureTx = txs}
    where
        txs = map matchTxs ls
        matchTxs ~[date, flag, _, desc, orig, _, _, _, val] =
          ItauTransaction {
              txData      = fromItauDate day Forward date
            , txDescricao = desc
            , txValor     = fromItauNum val
            , txOrigem    = orig
            , txFlag      = flag}

---------------------
-- Credit Card
---------------------

scrapAndBuildCCFatura :: Bool -> WD ItauCardFatura
scrapAndBuildCCFatura open = do
        mainItem <- findElem (ByCSS "div[id=\"TRNcontainer01\"]")
        htmlTables <- findElemsFrom mainItem (ByCSS "table[class=\"TRNfundo\"]")
        htmlTablesRows <- mapM (`findElemsFrom` ByTag "tr") htmlTables
        htmlTablesCells <- mapM rowsToCells htmlTablesRows
        tables <- mapM cellsToText htmlTablesCells
        -- liftIO $ print ("Tabelas" ::String) -- essential for debugging
        -- liftIO $ mapM_ print tables
        day <- liftIO today
        boxFatura <- findElem (ByCSS "div[id=\"divBoxFatura\"]")
        boxFaturaDivs <- findElemsFrom boxFatura (ByTag "div")
        boxFaturaDivsText <- mapM getText boxFaturaDivs
        return $ buildCCFatura day boxFaturaDivsText open tables
    where
        rowsToCells :: [Element] -> WD [[Element]]
        rowsToCells = mapM (`findElemsFrom` ByTag "td")
        cellsToText :: [[Element]] -> WD Table
        cellsToText = mapM (mapM getText)

buildCCFatura :: Day -> [Text] -> Bool -> [Table] -> ItauCardFatura
buildCCFatura day boxDivs open tables =
        emptyItauCardFatura {
              ccfAberta = open
            , ccfVencimento = getVencimento
            , ccfValorTotal = buildCCValorTotal tables
            , ccfTxs = buildCCFaturaTxs day tables
        }
    where
        getVencimento = fromItauDate day Backward $ dropWhile (/= "vencimento") boxDivs !! 1


buildCCValorTotal :: [Table] -> Float
buildCCValorTotal tables =
    negate . fromJust . fromJust $ find isJust (map buildCCValorTotal' tables)

buildCCValorTotal' :: Table -> Maybe Float
buildCCValorTotal' ([""]:[]:["Resumo da fatura em R$"]:xs) =
    Just . fromItauNum . last $ last xs
buildCCValorTotal' _ = Nothing

classificaCCParcelamento :: [ItauTransaction] -> [ItauTransaction]
classificaCCParcelamento txs =
        naoParcelados ++ geraParcelas [ (t, fromJust $ matchInfoParcelamento t) | t <- parcelados]
    where
        (parcelados, naoParcelados) = foldl (\(par, npar) tx -> if isTxParcelada tx then (tx:par, npar) else (par, tx:npar)) ([],[]) txs 

regexParcelada :: Regex
regexParcelada = mkRegex "^(.+)([0-9][0-9])/([0-9][0-9])$"

matchInfoParcelamento :: ItauTransaction -> Maybe [String]
matchInfoParcelamento tx = matchRegex regexParcelada $ T.unpack $ txDescricao tx

isTxParcelada :: ItauTransaction -> Bool
isTxParcelada = isJust . matchInfoParcelamento


geraParcelas :: [(ItauTransaction, [String])] -> [ItauTransaction]
geraParcelas xs = 
        concatMap geraTransacoesPara $ Map.elems txs        
    where
        txs = Map.fromList [((txData tx, T.strip $ T.pack desc), (read ptot::Integer, T.strip $ T.pack desc, tx)) | (tx, [desc, _, ptot]) <- xs]
        geraTransacoesPara (_, "DESCONTO ANUIDADE", tx) = [tx]
        geraTransacoesPara (ptot, desc, tx) =
            [ tx {
                txData = addGregorianMonthsClip p $ txData tx, 
                txDescricao = T.pack $ T.unpack desc ++ " - Parcela " ++ show (p + 1) ++ " de " ++ show ptot
                }    
            | p <- [0..ptot - 1]]

buildCCFaturaTxs :: Day -> [Table] -> [ItauTransaction]
buildCCFaturaTxs day tables =
        intlTxs ++ natlTxs
    where 
        intlTxs = buildCCFaturaInternationalTxs day tables
        natlTxs = foldl (buildCCFaturaNationalTxs day) [] tables
        
buildCCFaturaInternationalTxs :: Day -> [Table] -> [ItauTransaction]
buildCCFaturaInternationalTxs _   []     = []
buildCCFaturaInternationalTxs day tables =
        buildTxs start end ++ buildCCFaturaInternationalTxs day next
    where start = dropWhile (\tbl -> head tbl /= ["Lançamentos internacionais"]) tables
          end   = takeWhile (\l -> length l < 4) $ drop 1 start
          next  = dropWhile (\l -> length l < 4) $ drop 1 start
          buildTxs [] _ = []
          buildTxs (s:_) e =
              buildTxs' $ s  ++ concat e
          buildTxs' ~(["Lançamentos internacionais"]:[origin]:_header:lns) =
              fromJust . sequence . filter isJust $ map (toCCTx day origin) $ mergeIntlCCTx lns

buildCCFaturaNationalTxs :: Day -> [ItauTransaction] -> Table -> [ItauTransaction]
buildCCFaturaNationalTxs day acc (["Lançamentos nacionais"]:[origin]:_header:lns) =
    let txs = fromJust . sequence . filter isJust $ map (toCCTx day origin) lns
    in txs ++ acc
buildCCFaturaNationalTxs day acc (["Movimentações"]:_header:lns) =
    let lns1 = takeWhile (\l -> length l == 3) lns
    in map (fromJust . toCCTx day "") lns1 ++ acc
buildCCFaturaNationalTxs _ acc _ = acc


toCCTx :: Day -> Text -> [Text] -> Maybe ItauTransaction
toCCTx _ _ (" ":_) = Nothing
toCCTx day origin [date, desc, value] = toCCTx day origin [date, desc, value, ""]
toCCTx day origin [date, desc, value, flag] =
    Just ItauTransaction {
      txData      = fromItauDate day Backward date
    , txDescricao = desc
    , txValor     = negate $ fromItauNum value
    , txOrigem    = origin
    , txFlag      = flag
    }
toCCTx _ _ _ = Nothing

mergeIntlCCTx :: [[Text]] -> [[Text]]
mergeIntlCCTx lns = 
        mergeIntlCCTx' lns' []
    where
        lns' = filter (\l -> length l == 3) lns
        mergeIntlCCTx' ([dt1, de1, va1]:xs) acc
            | isBlank dt1 =
                let [dt0, de0, va0, dc0] = head acc in
                mergeIntlCCTx' xs ([dt0, wrapText de0 de1, va0, wrapText dc0 va1]: tail acc)
            | otherwise = mergeIntlCCTx' xs ([dt1, de1, va1, ""]:acc)
        mergeIntlCCTx' [] acc = acc
        mergeIntlCCTx' x  _   = error $ show x
        wrapText st nd 
            | isBlank nd = st
            | isBlank st = nd
            | otherwise  = T.concat [st, " - ", nd]

ccInfoSmokeTest :: ItauCardInfo -> Either Text ItauCardInfo
ccInfoSmokeTest = Right --todo

emptyItauCardInfo :: ItauCardInfo
emptyItauCardInfo =
    ItauCardInfo {
          ccProximaFatura       = emptyItauCardFatura
        , ccFaturaAtual         = emptyItauCardFatura
        , ccFaturaAnterior      = emptyItauCardFatura
        }

emptyItauCardFatura :: ItauCardFatura
emptyItauCardFatura =
    ItauCardFatura {
          ccfAberta = False
        , ccfVencimento = fromGregorian 1900 1 1
        , ccfValorTotal = 0
        , ccfTxs = []
        }

txToCSV :: ItauTransaction -> Text
txToCSV tx =
                             -- date; paymode; info  ; payee; memo; amount; category; tags
    T.concat $ intersperse ";" [dt  , "0"    , flag  , orig , desc, val   , ""      , ""]
    where
        dt   = pack . showGregorian $ txData tx
        desc = txDescricao tx
        val  = T.pack $ printf "%.2f" $ txValor tx
        orig = txOrigem tx
        flag = txFlag tx
