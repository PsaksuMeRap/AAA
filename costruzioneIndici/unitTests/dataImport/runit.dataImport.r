test.importReturnFile <- function() {
  basicPath  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/"
  sourceFile <- paste(basicPath,"importazione.r",sep="")
  rDataFile   <- paste(basicPath,"unitTests/dataImport/data/test.importedFiles.RData",sep="")
  
  myEnv <- new.env()

  source(sourceFile)
  load(rDataFile,envir=myEnv)

  myData <- importaHedgeFunds(estensione="CTA",verbose=FALSE) 
  checkEquals(myData$matrix, myEnv$importedFiles$matrix)
  checkEquals(myData$AUM.df, myEnv$importedFiles$AUM.df)
  checkEquals(myData$rendimentoForward.df, myEnv$importedFiles$rendimentoForward.df)
}

test.importLyxorIndices <- function() {
  basicPath  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/"
  sourceFile <- paste(basicPath,"importazione.r",sep="")
  rDataFile  <- paste(basicPath,"unitTests/dataImport/data/test.importedFilesLyxor.RData",sep="")
  dataFile   <- paste(basicPath,"unitTests/dataImport/data/LYXOR Indices Track Total new.csv",sep="")

  myEnv <- new.env()

  source(sourceFile)
  load(rDataFile,envir=myEnv)

  myData <- importLyxorIndices(dataFile) 

  checkEquals(myData, myEnv$testData)
}

