library("RUnit")
source("funzioni utilit tmp.r")
source("calcolo statistiche.r")
source("nuove strutture.r")


## test per l'importazione dei dati
testsuite.dataImport <- defineTestSuite("Test di importazione dati",
  dirs = "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/dataImport")

testResult <- runTestSuite(testsuite.dataImport); printTextProtocol(testResult)

library("RUnit")
## test di funzioni diverse
testsuite.functions <- defineTestSuite("Test alcune funzioni utili",
  dirs = "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/functions")

testResult <- runTestSuite(testsuite.functions); printTextProtocol(testResult)

library("RUnit")
## test delle funzione di calcolo delle statistiche
source("calcolo statistiche.r")
testsuite.calcoloStatistiche <- defineTestSuite("Test calcolo statistiche",
  dirs = "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/calcolo statistiche")

testResult <- runTestSuite(testsuite.calcoloStatistiche);printTextProtocol(testResult)


library("RUnit")
## test delle funzioni in "nuove strutture.r"
source("nuove strutture.r")
source("importazione.r")
testsuite.nuoveStrutture <- defineTestSuite("Test nuove strutture",
  dirs = "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture")

testResult <- runTestSuite(testsuite.nuoveStrutture);printTextProtocol(testResult)

