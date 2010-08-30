CreateIndexTimeSeriesPrice <- function() {
  # crea una lista contenente le serie storiche dei prezzi degli strumenti 
  # da utilizzare nella costruzione dell'indice
  timeSeries <-list()
  class(timeSeries) <- "indexTimeSeriesPrice"
  return(timeSeries)
}

AddToIndexTimeSeriesPrice <- function(index,yyyymmdd,newConstituents) {
  # aggiunge ad un indice di tipo indexTimeSeriesPrice le osservazioni 
  # alla data yyyymmdd contenute nel data.frame newconstituents
  
  # index: una variabile di tipo indexTimeSeriesPrice
  # yyyymmdd: una stringa contenente una data in formato yyyy-mm-dd
  # newConstituents: data.frame con due campi: "name" e "price"
 
  # if (exists(yyyymmdd,where=index)) {
    if (!is.null(index[[yyyymmdd]])) {
    actual.df <- index[[yyyymmdd]]$constituents
    areToRemove <- is.element(actual.df[,"name"],newConstituents[,"name"])

    if (any(areToRemove)) actual.df <- actual.df[!areToRemove,,drop=FALSE]
      index[[yyyymmdd]] <- list(yyyymmdd=yyyymmdd,
                                constituents=rbind(actual.df,newConstituents))
  } else { 
    index[[yyyymmdd]] <- list(yyyymmdd=yyyymmdd,constituents=newConstituents)
  }
  return(index)
}

CreateIndexConstituents <- function(name, quantity=rep(0,length(name))) {
  if (any(is.na(name))) stop("Error: NA name in function CreateIndexConstituent.")
  if (any(!is.character(name))) stop ("Error: no character type for name in function CreateIndexConstituent.")
  if (any(name == "")) stop("Error: empty name in function CreateIndexConstituent.")
  if (any(is.na(quantity))) stop("Error: NA quantity in function CreateIndexConstituent.")
  if (any(!is.numeric(quantity))) stop ("Error: no numeric type for quantity in function CreateIndexConstituent.")
  if (length(name) != length(quantity)) stop("Error: name and quantity have different lenght.")

  indexConstituents <- data.frame(name=name,quantity=quantity,stringsAsFactors=FALSE)
 
  return(indexConstituents)
}

CreateIndex <- function() { 
  index <- new.env()
  class(index) <- "index"
  return(index)
}

AddConstituentsToIndex <- function(index,yyyymmdd,newConstituents) { 
  if (exists(yyyymmdd,where=index)) {
    actual.df <- index[[yyyymmdd]]$constituents
    areToRemove <- is.element(actual.df[,"name"],newConstituents[,"name"])
    if (any(areToRemove)) actual.df <- actual.df[!areToRemove,,drop=FALSE]
      index[[yyyymmdd]] <- list(yyyymmdd=yyyymmdd,
                                constituents=rbind(actual.df,newConstituents))
  } else { 
    index[[yyyymmdd]]<-list(yyyymmdd=yyyymmdd,constituents=newConstituents)
  }
  return(index)
}

ImportConstituentsPrices <- function(dataFile) {

  #depends from source("importazione.r")

  datiIndice.df <- importLyxorIndices(dataFile)
  datiIndice.df <- datiIndice.df[-1, ,drop=FALSE]
  datiIndice.m <- t(datiIndice.df)

  dates.v <- colnames(datiIndice.m)
  names.v <- rownames(datiIndice.m)
  constituentsPrices <- CreateIndexTimeSeriesPrice()

  for (i in 1:ncol(datiIndice.m)) {
    prices.v <- datiIndice.m[,i]
    names(prices.v) <- NULL
    prices.df <- data.frame(name=names.v,price=prices.v)
    constituentsPrices <- AddToIndexTimeSeriesPrice(constituentsPrices,dates.v[i],prices.df)
    }

  return(constituentsPrices)
}

AvailableDates <- function(constituentsPrices) {
  return(names(constituentsPrices))
}

GetStartDate <- function(constituentsPrices) {
  myDates <- names(constituentsPrices)
  dates <- as.Date(myDates,format="%Y-%m-%d")
  start.date <- as.character(min(dates))
  endDate <- as.character(max(dates))

  return(c(start.date,endDate))
}

NumberObservations <- function(timeSeries) {
  return(sapply(timeSeries,function(ts) return(nrow(ts$price))))
}


CreateFinancialTimeSeries <- function(name,dates.v,data.v) {
  # crea una lista contenente il nome della serie, il vettore di date in formato
  # yyyy-mm-dd e le osservazioni dei prezzi o dei rendimenti dates.v e data.v devono
  # avere la medesima lunghezza.
  ts <- list()

  ts$name <- name
  ts$dates.v <- dates.v
  ts$data.v <- data.v

  ts[["IsObservationAvailableAtDate"]] <- function(date) {
    # una stringa di caratteri data in formato yyyy-mm-dd
    date <- as.character(date)
    isMatched <- ts$dates.v == date
    if (any(isMatched)) return(!is.na(ts$data.v[isMatched])) else return(FALSE)
  }

  ts[["IsInSet"]] <- function(set) {
    # timeSeries: una variabile di tipo financialTimeSeries
    # set: un vettore di nomi
    return(is.element(ts$name,set))
  }

  ts[["SimpleReport"]] <- function() {
    isAvailable <- !is.na(ts$data.v)
    datesWithAvailableData <- ts$dates.v[isAvailable]
    if (length(datesWithAvailableData) == 0) {
      outputText <- "no data available :"
      outputText <- paste(outputText, ts$name)
      return(outputText)
    }
  
    start.date <- datesWithAvailableData[1]
    endDate <- datesWithAvailableData[length(datesWithAvailableData)]
    outputText <- paste(start.date,"/",endDate)
    outputText <- paste(outputText," : ",ts$name,sep="")
    return(outputText)  
  }

  ts[["methods"]] <- c("IsObservationAvailableAtDate","IsInSet",
                       "SimpleReport","methods")
  
  class(ts) <- "financialTimeSeries" 

  return(ts)
}

ImportaPrezziDaCsv <- function(fileName) {
  # questa funzione importa i dati presenti in un file csv in una lista i cui
  # elementi sono di tipo financialTimeSeries

  wrapper <- function(i,nomiFondi,dates.v,data.df) {
    return(CreateFinancialTimeSeries(name=nomiFondi[i],dates.v,data.v=data.df[,i]))
  }

  ## importa gli header
  nomiFondi <- read.csv(file=fileName, header=FALSE,nrows=1,colClasses="character",stringsAsFactors=FALSE)
  nomiFondi <- unlist(nomiFondi[1,-1,drop=FALSE],use.names=FALSE)

  ## importa tutta la tabella  
  data.df <- read.csv(file=fileName, header=FALSE, skip=1,stringsAsFactors=FALSE)

  ## crea il vettore di date
  dates.v <- as.Date(data.df[[1]],"%m/%d/%Y")

  ## elimina la prima colonna che non contiene una data
  data.df <- data.df[,-1,drop=FALSE]

  ## crea la lista delle financialTimeSeries
  finTimeSeries.l <- lapply(1:length(nomiFondi),wrapper,nomiFondi,dates.v,data.df)

  return(finTimeSeries.l)
}

NumberObservationsAvailableAtDate <- function(timeSeries.l,date) {
  # timeSeries.l: una lista di variabili di tipo financialTimeSeries
  # una stringa di caratteri data in formato yyyy-mm-dd
  wrapper <- function(ts,date) return(ts$IsObservationAvailableAtDate(date)) 
  isValid <- sapply(timeSeries.l,wrapper,date)
  return(sum(isValid))
}


AreTimeSeriesInSet <- function(timeSeries.l,set) {
  # timeSeries.l: una lista di variabili di tipo financialTimeSeries
  # set: un vettore di nomi
  
  wrapper <- function(ts,set) return(ts$IsInSet(set))
  areInSet <- sapply(timeSeries.l,wrapper,set)
  return(areInSet)
}


SimpleReportTimeSeries <- function(timeSeries.l) {
  wrapper <- function(ts) return(ts$SimpleReport())
  outputs <- sapply(timeSeries.l,wrapper)
  return(outputs)
}


ExtractTimeSeries <- function(seriesNames,timeSeries.l,simplify=TRUE) {
  toExtract <- AreTimeSeriesInSet(timeSeries.l,seriesNames)
  nbElementsToExtract <- sum(toExtract)
  if (nbElementsToExtract>1)  return(timeSeries.l[toExtract])
  if (nbElementsToExtract==1) {
     indexToExtract <- (1:length(toExtract))[toExtract]
     if (simplify) {
       return(timeSeries.l[[indexToExtract]]) 
     } else {
       return(timeSeries.l[indexToExtract])
     }
  }
  return(NULL)
}


ExtractPriceAtDate <- function(timeSeries.l,desiredDate,desiredSeries) {
  # timeSeries.l: a list of FinancialTimeSeries
  # the date at which to extract the prices
  # desiredSeries: (optional) the names of the series to ExtractPriceAtDate

  if (!missing(desiredSeries)) {
    result <- rep(NA_real_,length(desiredSeries))
    names(result) <- desiredSeries
    timeSeries.l <- ExtractTimeSeries(desiredSeries,timeSeries.l,simplify=FALSE)
    if (length(timeSeries.l)==0) return(result)
    
  }

  extract <- function(series, desiredDate) {
    isToExtract <- series$dates.v == desiredDate
    if (any(isToExtract)) return(series$data.v[isToExtract])
    return(NA_real_)
  }
  prices <- sapply(timeSeries.l,extract,desiredDate)
  namesTimeSeriesUsed <- ExtractTimeSeriesNames(timeSeries.l)
  names(prices) <- namesTimeSeriesUsed

  if (!missing(desiredSeries)) {
    result[namesTimeSeriesUsed] <- prices
    return(result)
  } else {
    return(prices)
  }
}


ExtractTimeSeriesNames <- function(timeSeries.l) {
  getName <- function(ts) return(ts$name)
  outputs <- sapply(timeSeries.l,getName)
  return(outputs)
}

CreateIndex <- function(name) {

  index <- list()
  index[["name"]] <- name

  index[["data"]] <- list()

  index[["InitializeByWeights"]] <- function(startingDate,weights,timeSeries.l) {
    price.v <- ExtractPriceAtDate(timeSeries.l,startingDate)
    if (any(is.na(price.v))) stop("Error: price with NA")

    orderedNames <- names(weights)
    if (is.null(orderedNames)) stop("Error: weights without names")

    orderedPrices <- price.v[orderedNames] 
    quantities = 100.0 * weights / orderedPrices
    # result.df <- data.frame(weights=weights,quantities=quantities,prices=orderedPrices)
    result.df <- data.frame(weights=weights,quantities=quantities)
    rownames(result.df) <- orderedNames
    index[["data"]][[startingDate]] <- result.df
    return(result.df)
  }   
}

InitializeIndexByWeights <- function(startingDate,weights,timeSeries.l) {
  price.v <- ExtractPriceAtDate(timeSeries.l,startingDate)
  if (any(is.na(price.v))) stop("Error: price with NA")

  orderedNames <- names(weights)
  if (is.null(orderedNames)) stop("Error: weights without names")

  orderedPrices <- price.v[orderedNames] 
  quantities = 100.0 * weights / orderedPrices
 # result.df <- data.frame(weights=weights,quantities=quantities,prices=orderedPrices)
  result.df <- data.frame(weights=weights,quantities=quantities)
  rownames(result.df) <- orderedNames
  return(result.df)
} 


InitializeIndexByQuantities <- function(startingDate,quantities,timeSeries.l) {
  price.v <- ExtractPriceAtDate(timeSeries.l,startingDate)

  if (any(is.na(price.v))) stop("Error: price with NA")
  orderedNames <- names(quantities)
  orderedPrices <- price.v[orderedNames]
  weights = quantities*orderedPrices  
  indexValue = sum(weights)
  weights = weights / indexValue 
 # result.df <- data.frame(weights=weights,quantities=quantities,prices=orderedPrices)
  result.df <- data.frame(weights=weights,quantities=quantities)
  rownames(result.df) <- orderedNames
  return(result.df)
} 
