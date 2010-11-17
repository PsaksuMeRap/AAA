test.CreateIndexTimeSeriesPrice <- function() {
  timeSeries <- CreateIndexTimeSeriesPrice()
  tmp <- list(); class(tmp) <- "indexTimeSeriesPrice"

  checkEquals(class(timeSeries),"indexTimeSeriesPrice")
  checkEquals(timeSeries,tmp) 
  checkEquals(is.list(timeSeries),TRUE) 
}

test.AddToIndexTimeSeriesPrice <- function() {
  start.date <- data.frame(name="Claudio", price=42.0)

  data.to.add <- data.frame(name=c("Luca","Maria"),price=c(3.5,6))

  data.complete <- rbind(start.date,data.to.add)

  data.to.add.and.replace <- data.frame(name=c("Luca","Maria","Carlo"), price=c(3,4,5.6))

  data.added.and.replaced <- data.complete
  data.added.and.replaced[2,"price"] = 3
  data.added.and.replaced[3,"price"] = 4
  data.added.and.replaced <- rbind(data.added.and.replaced,data.frame(name="Carlo",price=5.6))

  
  myIndex1 <- CreateIndexTimeSeriesPrice()
  myIndex1[["2002-01-05"]] <- list(yyyymmdd="2002-01-05",constituents=start.date)

  myIndex2 <- CreateIndexTimeSeriesPrice()
  myIndex2[["2002-01-05"]] <- list(yyyymmdd="2002-01-05",constituents=data.complete)
  
  myIndex3 <- myIndex2 
  myIndex3[["2008-05-12"]] <- list(yyyymmdd="2008-05-12",constituents=data.to.add)

  myIndex4 <- myIndex3
  myIndex4[["2002-01-05"]] <- list(yyyymmdd="2002-01-05",constituents=data.added.and.replaced)

  index <- CreateIndexTimeSeriesPrice()

  # the index is empty
  index <- AddToIndexTimeSeriesPrice(index,yyyymmdd="2002-01-05",newConstituents=start.date)
  checkEquals(index,myIndex1)

  # when the Index still contains other constituents
  index <- AddToIndexTimeSeriesPrice(index,yyyymmdd="2002-01-05",newConstituents=data.to.add)
  checkEquals(index,myIndex2)
  
  # with new dates 
  index <- AddToIndexTimeSeriesPrice(index,yyyymmdd="2008-05-12",newConstituents=data.to.add)
  checkEquals(index,myIndex3)

  # replace some entries
  index <- AddToIndexTimeSeriesPrice(index,yyyymmdd="2002-01-05",newConstituents=data.to.add.and.replace)
  checkEquals(index,myIndex4)

  # check the class
  checkEquals(class(index),"indexTimeSeriesPrice")
}


test.ImportConstituentsPrices <- function() {

  #depends from source("importazione.r")
  basic.path  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture/dati/"
  R.data.file  <- paste(basic.path,"importConstituentsPrices.RData",sep="")

  csv.file <- "LYXOR Indices Track Total new.csv"
  data.file <- paste(basic.path,csv.file,sep="")

  myEnv <- new.env()
  load(R.data.file,envir=myEnv)

  constituentsPrices <- ImportConstituentsPrices(data.file)
  checkEquals(constituentsPrices,myEnv$indexPrices)
  checkEquals(class(constituentsPrices),"indexTimeSeriesPrice")

}


test.CreateFinancialTimeSeries <- function() {
  name <- "Serie di test"
  dates.v <- c("2010-11-12","2010-11-17","2010-11-24")
  data.v <- c(1.0,2.0,0)

  nuovaSerie <- CreateFinancialTimeSeries(name,dates.v,data.v) 

  checkEquals(nuovaSerie$name,name)
  checkEquals(nuovaSerie$dates.v,dates.v)
  checkEquals(nuovaSerie$data.v,data.v)
  checkEquals(class(nuovaSerie),"financialTimeSeries")
  checkEquals(nuovaSerie$methods,c("IsObservationAvailableAtDate","IsInSet",
                  "SimpleReport","methods"))
}


test.ImportaPrezziDaCsv <- function() {
  
  basic.path  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture/dati/"
  R.data.file  <- paste(basic.path,"importaPrezziDaCsv.RData",sep="")
  file.name   <- paste(basic.path,"LYXOR Indices Track Total new.csv", sep="")

  myEnv <- new.env()

  load(R.data.file,envir=myEnv)
  my.data <- ImportaPrezziDaCsv(file.name) 

  checkEquals(my.data[[1]]$name, myEnv$a[[1]]$name)
  checkEquals(my.data[[1]]$dates.v, myEnv$a[[1]]$dates.v)
  checkEquals(my.data[[1]]$data.v, myEnv$a[[1]]$data.v)
  checkEquals(class(my.data[[1]]), "financialTimeSeries")

  checkEquals(my.data[[3]]$name, myEnv$a[[3]]$name)
  checkEquals(my.data[[3]]$dates.v, myEnv$a[[3]]$dates.v)
  checkEquals(my.data[[3]]$data.v, myEnv$a[[3]]$data.v)
  checkEquals(class(my.data[[3]]), "financialTimeSeries")
}

test.IsObservationAvailableAtDate <- function() {

  basic.path  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture/dati/"
  R.data.file  <- paste(basic.path,"importaPrezziDaCsv.RData",sep="")
  myEnv <- new.env()
  load(R.data.file,envir=myEnv)

  ts1 <- myEnv$a[[1]]
  ts2 <- myEnv$a[[2]]
  
  x <- ts1$IsObservationAvailableAtDate("1998-01-02")  # data specificata non disponibile
  y <- ts1$IsObservationAvailableAtDate("2002-05-07")  # data specificata disponibile, osservazione disponibile
  z <- ts2$IsObservationAvailableAtDate("2002-04-09")  # data specificata disponibile, osservazione non disponibile
  w <- ts2$IsObservationAvailableAtDate("2003-11-18")  # data specificata disponibile, osservazione disponibile

  checkEquals(x,FALSE)
  checkEquals(y,TRUE)
  checkEquals(z,FALSE)
  checkEquals(w,TRUE)
}

test.NumberObservationsAvailableAtDate <- function() {

  basic.path  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture/dati/"
  R.data.file  <- paste(basic.path,"importaPrezziDaCsv.RData",sep="")
  myEnv <- new.env()
  load(R.data.file,envir=myEnv)

  ts.l <- myEnv$a
  
  x <- NumberObservationsAvailableAtDate(ts.l,"1998-01-02")  # data specificata non disponibile
  y <- NumberObservationsAvailableAtDate(ts.l,"2002-05-07")  # data specificata disponibile, osservazione disponibile
  z <- NumberObservationsAvailableAtDate(ts.l,"2002-04-09")  # data specificata disponibile, osservazione non disponibile
  w <- NumberObservationsAvailableAtDate(ts.l,"2003-11-18")  # data specificata disponibile, osservazione disponibile

  checkEquals(x,0)
  checkEquals(y,18)
  checkEquals(z,17)
  checkEquals(w,18)
}


test.CreateIndexConstituents <- function() {

  name1 <- NA        ## create exception because NA name
  name2 <- 5.5       ## create exception because not character name
  name3 <- ""        ## create exception because empty name
  name4 <- "Claudio" ## ok
  quantity1 <- NA    ## create exception because NA quantity
  quantity2 <- ""    ## create exception because not numeric quantity
  quantity3 <- 4.5   ## ok
 
  # single constituend
  resultName4 <- data.frame(name=name4,quantity=0,stringsAsFactors=FALSE);
  resultQuantity3 <- data.frame(name=name4,quantity=quantity3,stringsAsFactors=FALSE);

  # single constituend's checks
  checkException(CreateIndexConstituents(name=name1),silent=TRUE)
  checkException(CreateIndexConstituents(name=name2),silent=TRUE)
  checkException(CreateIndexConstituents(name=name3),silent=TRUE)
  checkEquals(CreateIndexConstituents(name=name4),resultName4)

  checkException(CreateIndexConstituents(name=name4,quantity=quantity1),
		  silent=TRUE)
  checkException(CreateIndexConstituents(name=name4,quantity=quantity2),
		  silent=TRUE)
  checkEquals(CreateIndexConstituents(name=name4,quantity=quantity3),
              resultQuantity3)

  # many constituents
  manyNames1 <- c("Claudio","Luca","Gianni") # ok
  manyQuantity1 <- c(1,2,3)

  manyNames2 <- c("Claudio",NA,"Gianni") # NA in names
  manyQuantity2 <- c(1,2,3)

  manyNames3 <- c("Claudio","Luca","Gianni") # NA in quantity
  manyQuantity3 <- c(1,2,NA)

  manyNames4 <- c("Claudio","Luca","") # empty name
  manyQuantity4 <- c(1,2,3)  

  manyNames5 <- c("Claudio","Luca","") # name and quantity different length
  manyQuantity5 <- c(1,2) 

  manyResult1 <- data.frame(name=manyNames1,quantity=manyQuantity1,stringsAsFactors=FALSE);

  # many constituents checks
  checkException(CreateIndexConstituents(name=manyNames2,silent=TRUE))
  checkException(CreateIndexConstituents(name=manyNames2,quantity=manyQuantity2),
		  silent=TRUE)
  checkException(CreateIndexConstituents(name=manyNames3,quantity=manyQuantity3),
		  silent=TRUE)
  checkException(CreateIndexConstituents(name=manyNames4,quantity=manyQuantity4),
		  silent=TRUE)
  checkException(CreateIndexConstituents(name=manyNames5,quantity=manyQuantity5),
		  silent=TRUE)
  checkEquals(CreateIndexConstituents(name=manyNames1,quantity=manyQuantity1),manyResult1)

}


test.CreateIndex <- function() {
  index <- new.env()
  class(index) <- "index"

  checkEquals(CreateIndex(),index)
}

test.AddConstituentsToIndex <- function() {
  name = "Claudio"
  quantity = 42.0

  namesToAdd <- c("Luca","Maria")
  quantityToAdd <- c(3,4)

  names <- c(name,namesToAdd)
  quantities <- c(quantity,quantityToAdd)

  namesToAddAndReplace <- c("Luca","Maria","Carlo")
  quantityToAddAndReplace <- c(3,4,5.6)

  nemesAddedAndReplaced <- c(name,namesToAddAndReplace)
  quantityAddedAndReplaced <- c(quantity,quantityToAddAndReplace)

  indexConstituent <- CreateIndexConstituents(name=name,quantity=quantity)
  indexConstituentsToAdd <- CreateIndexConstituents(name=namesToAdd,quantity=quantityToAdd)
  indexConstituentsComplete <- CreateIndexConstituents(name=names,quantity=quantities)
  indexConstituentsToAddAndReplaced <- CreateIndexConstituents(name=namesToAddAndReplace,
                                       quantity=quantityToAddAndReplace)
  indexConstituentsAddedAndReplaced <- CreateIndexConstituents(name=nemesAddedAndReplaced,
                                       quantity=quantityAddedAndReplaced)

  myIndex1 <- CreateIndex()
  myIndex1[["2002-01-05"]] <- list(yyyymmdd="2002-01-05",constituents=indexConstituent)

  myIndex2 <- CreateIndex()
  myIndex2[["2002-01-05"]] <- list(yyyymmdd="2002-01-05",constituents=indexConstituentsComplete)
  
  myIndex3 <- myIndex2
  myIndex3[["2008-05-12"]] <- list(yyyymmdd="2008-05-12",constituents=indexConstituentsToAdd)

  myIndex4 <- myIndex3
  myIndex4[["2002-01-05"]] <- list(yyyymmdd="2002-01-05",
                                   constituents=indexConstituentsAddedAndReplaced)

  index <- CreateIndex()

  # the index is empty
  index <- AddConstituentsToIndex(index,yyyymmdd="2002-01-05",newConstituents=indexConstituent)
  checkEquals(index,myIndex1)

  # when the Index still contains other constituents
  index <- AddConstituentsToIndex(index,yyyymmdd="2002-01-05",newConstituents=indexConstituentsToAdd)
  checkEquals(index,myIndex2)
  
  # with new dates 
  index <- AddConstituentsToIndex(index,yyyymmdd="2008-05-12",newConstituents=indexConstituentsToAdd)
  checkEquals(index,myIndex3)
  
  # replace some entries
  index <- AddConstituentsToIndex(index,yyyymmdd="2002-01-05",newConstituents=indexConstituentsToAddAndReplaced)
  checkEquals(index,myIndex4)

}

test.AvailableDates <- function() {

  basic.path  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture/dati/"
  R.data.file  <- paste(basic.path,"importConstituentsPrices.RData",sep="")
  myEnv <- new.env()
  load(R.data.file,envir=myEnv)

  myNames <- c("2002-04-09","2002-04-16","2002-04-23","2002-04-30")
  myNames <- names(myEnv$indexPrices[1:4])
  checkEquals(AvailableDates(myEnv$indexPrices[1:4]),myNames)
}

test.GetStartDate <- function() {

  basic.path  <- "/home/claudio/Dropbox/analisi serie storiche/capitalgest/unitTests/nuove strutture/dati/"
  R.data.file  <- paste(basic.path,"importConstituentsPrices.RData",sep="")
  myEnv <- new.env()
  load(R.data.file,envir=myEnv)

  start.date <- "2002-04-09"
  end.date   <- "2010-02-23"

  checkEquals(GetStartDate(myEnv$indexPrices),c(start.date,end.date))
}

test.NumberObservations <- function() {
  timeSeries <- list()
  class(timeSeries) <- "indexTimeSeriesPrice"
  
  timeSeries[["2002-04-09"]] <- list(yyyymmdd="2002-04-09",price=data.frame(name="Claudio",price=105.2))
  timeSeries[["2002-04-16"]] <- list(yyyymmdd="2002-04-16",
                                   price=data.frame(name=c("Claudio","Luca"),price=c(105.2,123.8)))
  
  nbObs.v <- NumberObservations(timeSeries)
  expectedResult <- c(1,2)
  names(expectedResult) <- c("2002-04-09","2002-04-16")
  checkEquals(nbObs.v,expectedResult)
}


test.IsTimeSeriesNameInSet <- function() {
  name <- "pippo"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(5.14,12.88)
  timeSeries <- CreateFinancialTimeSeries(name,dates.v,data.v)
  result1 <- timeSeries$IsInSet(set="pippo")
  result2 <- timeSeries$IsInSet(set=c("pipp","aiuto"))
  checkEquals(result1,TRUE)
  checkEquals(result2,FALSE)
}

test.AreTimeSeriesInSet <- function() {
  name <- "samia"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(5.14,12.88)
  

  timeSeries.l <- list()
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name="pippo",dates.v,data.v)

  set <- c("claudio","luca","reto","samia")
  areInSet <- AreTimeSeriesInSet(timeSeries.l,set)
  checkEquals(areInSet,c(TRUE,FALSE))

  set <- c("pippo","samia")
  areInSet <- AreTimeSeriesInSet(timeSeries.l,set) 
  checkEquals(areInSet,c(TRUE,TRUE))
}

test.ExtractTimeSeries <- function() {
  timeSeries.l <- list()
  name <- "claudio"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(5.14,12.88)
  
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  dates.v <- c("2010-03-14","2010-03-21","2010-05-12")
  data.v  <- c(5.14,12.88,55.33456)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name="pippo",dates.v,data.v)
  dates.v <- c("2010-03-14")
  data.v  <- 1.0
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name="Lindo",dates.v,data.v)  

  expectedResult1 <- timeSeries.l[[3]]
  expectedResult2 <- timeSeries.l[2:3]
  expectedResult3 <- NULL
  expectedResult4 <- NULL
  expectedResult5 <- timeSeries.l[3]

  checkEquals(ExtractTimeSeries(c("Lindo"),timeSeries.l),expectedResult1)
  checkEquals(ExtractTimeSeries(c("pippo","Lindo"),timeSeries.l),expectedResult2)
  checkEquals(ExtractTimeSeries(c("Arturo"),timeSeries.l),expectedResult3)
  checkEquals(ExtractTimeSeries(c("Arturo"),timeSeries.l,simplify=FALSE),expectedResult4)
  checkEquals(ExtractTimeSeries(c("Lindo"),timeSeries.l,simplify=FALSE),expectedResult5)
}

test.SimpleReport <- function() {
  timeSeries.l <- list()
  name <- "Claudio"
  dates.v <- c("2010-03-14","2010-03-21","2010-03-28")
  data.v  <- c(5.14,12.88,NA)
  timeSeries <- CreateFinancialTimeSeries(name,dates.v,data.v)

  should <- "2010-03-14 / 2010-03-21 : Claudio"
  checkEquals(timeSeries$SimpleReport(),should)

  data.v  <- rep(NA_real_,3)
  timeSeries <- CreateFinancialTimeSeries(name,dates.v,data.v)
  should <- "no data available : Claudio"
  checkEquals(timeSeries$SimpleReport(),should)
}


test.SimpleReportTimeSeries <- function() {
  timeSeries.l <- list()
  name <- "Claudio"
  dates.v <- c("2010-03-14","2010-03-21","2010-03-28")
  data.v  <- c(5.14,12.88,33.6)
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  
  name <- "Luca"
  data.v  <- c(5.0,NA,12)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name,dates.v,data.v)

  name <- "Maria"
  data.v  <- c(NA,22,44.6)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)  

  outputs <- SimpleReportTimeSeries(timeSeries.l)

  should <- c("2010-03-14 / 2010-03-28 : Claudio",
              "2010-03-14 / 2010-03-28 : Luca",
              "2010-03-21 / 2010-03-28 : Maria")
  checkEquals(outputs,should)
}


test.ExtractPriceAtDate <- function() {
  timeSeries.l <- list()

  name <- "Claudio"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(5.14,12.88)
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  
  name <- "Luca"
  data.v  <- c(5,25.0)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name,dates.v,data.v)

  name <- "Maria"
  data.v  <- c(12,22)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)  


  result <- ExtractPriceAtDate(timeSeries.l,"2010-03-21")
  should <- c(12.88,25,22)
  names(should) <- c("Claudio","Luca","Maria")

  checkEquals(result,should)

  should <- c(22,25,12.88)
  names(should) <- c("Maria","Luca","Claudio")
  result <- ExtractPriceAtDate(timeSeries.l,"2010-03-21",
                  desiredSeries=c("Maria","Luca","Claudio"))

  checkEquals(result,should)

  name <- "Maria"
  dates.v <- c("2010-03-14")
  data.v  <- c(5.14)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  should <- c(12.88,25,22)
  names(should) <- c("Claudio","Luca","Maria")
  should["Maria"] <- NA_real_
  result <- ExtractPriceAtDate(timeSeries.l,"2010-03-21")
  checkEquals(result,should)
  
  name <- "Maria"
  dates.v <- c("2010-03-14","2010-03-21","2010-03-28")
  data.v  <- c(NA_real_,5.14,12)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  should <- c(NA_real_,NA_real_,5.14,5,NA_real_)
  names(should) <- c("Ma","Maria","Claudio","Luca","pa")
  result <- ExtractPriceAtDate(timeSeries.l,"2010-03-14",
            desiredSeries=c("Ma","Maria","Claudio","Luca","pa"))
  checkEquals(result,should)

}

test.InitializeIndexByWeights <- function() {

  timeSeries.l <- list()
  name <- "Claudio"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(5.14,12.88)
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  
  name <- "Luca"
  data.v  <- c(5,25.0)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name,dates.v,data.v)

  name <- "Maria"
  data.v  <- c(12,22)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)  

  weights <- rep(1/3,3)
  names(weights) <- c("Claudio","Luca","Maria")
  start.date <- "2010-03-21"
  
  result <- InitializeIndexByWeights(start.date,weights,timeSeries.l)
  should <- data.frame(weights=rep(1/3,3),quantities=100 / 3 * c(1/12.88,1/25,1/22))
  rownames(should) <- c("Claudio","Luca","Maria")
  checkEquals(result,should)
  
  timeSeries.l[[3]]$data.v[2] <- NA_real_
  checkException(InitializeIndexByWeights(start.date,weights,timeSeries.l))
}


test.InitializeIndexByQuantities <- function() {

  timeSeries.l <- list()
  name <- "Claudio"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(5.14,12.88)
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  
  name <- "Luca"
  data.v  <- c(5,25.0)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name,dates.v,data.v)

  name <- "Maria"
  data.v  <- c(12,22)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)  

  quantities <- c(1,2,3)
  prices <- c(25.0,12.88,22.0)
  names(quantities) = c("Luca","Claudio","Maria")
  weights <- quantities * prices
  indexValue = sum(weights)
  weights <- weights/indexValue
  start.date <- "2010-03-21"
  
  result <- InitializeIndexByQuantities(start.date,quantities,timeSeries.l)
  #should <- data.frame(weights=weights,quantities=c(1,2,3),prices=prices)
  should <- data.frame(weights=weights,quantities=c(1,2,3))
  rownames(should) <- c("Luca","Claudio","Maria")
  checkEquals(result,should)
  
  timeSeries.l[[3]]$data.v[2] <- NA_real_
  checkException(InitializeIndexByWeights(start.date,weights,timeSeries.l))
}

CreateTestTimeSeries <- function() {
  # funzione non da testare utilizzata nei test
  timeSeries.l <- list()
  # first series
  name <- "Claudio"
  dates.v <- c("2010-03-14","2010-03-21")
  data.v  <- c(1.0,2.0)
  timeSeries.l[[1]] <- CreateFinancialTimeSeries(name,dates.v,data.v)
  # second series
  name <- "Luca"
  data.v  <- c(2.0,1.0)
  timeSeries.l[[2]] <- CreateFinancialTimeSeries(name,dates.v,data.v)

  name <- "Maria"
  data.v  <- c(3.0,3.0)
  timeSeries.l[[3]] <- CreateFinancialTimeSeries(name,dates.v,data.v)  

  return(timeSeries.l)
}

test.ComputeIndexByWeights <- function() {

return()  
  timeSeries.l <- CreateTestTimeSeries()

  weights <- rep(1/3,3)
  names(weights) <- c("Claudio","Luca","Maria")
  names.weights <- c("Claudio","Luca","Maria")

  start.date <- "2010-03-14"
  end.date <- "2010-03-21"


  
  # calcolo del should (eseguito a mano separatamente)
  should.new.index.value <- 350/3
  should.new.quantities.v <- c(should.new.index.value/2/3,should.new.index.value/3,
                               should.new.index.value/3/3)
  names(should.new.quantities.v) <- names.weights

  index.initialized <- InitializeIndexByWeights(start.date,weights,timeSeries.l)
  # calcola il valore attuale dell'indice
  # ci sono tutti i prezzi nuovi?

  ComputeIndexByWeights <- function(start.date,end.date,index.l,weights.v,timeSeries.l) {
    # start.dates
    # end.dates
    # index.l : list con variabili $date e $data.dat
    # weights.v : named vector
    # timeSeries.l : named list delle serie storiche dei prezzi (ogni elemento 
    #                della lista è una serie storica.
 
    is.observed.v <- sapply(timeSeries.l,IsObservationAvailableAtDate,end.date)
    names.series.with.observation <- ExtractTimeSeriesNames(timeSeries.l)[is.observed.v]
    names.weights <- names(weights)
 
    index.at.start.date <- ExtractIndexAtDate(start.date,index.l)


# index.l <- list(
#                 list(date="2010-04-18",data=dat1),
#                 list(date="2010-04-25",data=dat2),
#                 ...
#               )
# names(index.l) <- c("2010-04-18","2010-04-25")


    names.series.in.index <- rownames(index.at.start.date$dat)
  
    prices.old.v  <- ExtractPriceAtDate(timeSeries.l,start.date,desiredSeries=names.series.in.index)

  if (any(is.na(prices.old.v))) stop("Nell'indice sono presenti serie senza osservazioni alla data posteriore")

  pricesNew.v <- ExtractPriceAtDate(timeSeries.l,end.date,desiredSeries=names.weights)
  if (any(is.na(pricesNew.v))) stop("Nell'indice sono richieste serie senza osservazioni alla data attuale")

  new.index.value = sum(pricesNew.v*index.initialized[names.weights,"quantities"])  
  invested.amounts.v <- weights*new.index.value
  new.quantities.v <- invested.amounts.v / pricesNew.v
  delta.quantities.v <- new.quantities.v - index.initialized[names.weights,"quantities"]
  output.df <- data.frame(weights=weights,quantity=new.quantities.v)
  rownames(output.df) <- names(weights)
  }

  checkEquals(should.new.index.value,new.index.value)
  # print(list(structure=output.df,deltaQuantities=delta.quantities.v))
}
