## uncomment to debug
#rm(list=ls(all=TRUE))
#setwd("C:\\R\\r-output")


load("excel_input_cov_matrix.RData")
library(tcltk)
source("./lib/utilities.R")
source("../libraries/lib/utilities/datesUtilities.R")


exitWithError = 0


dailyToMonthly <- function(dataFrame,endOfMonth=FALSE)
{
  ## cosa fa questa funzione?
  ## dataFrame: a data.frame where the unique column of dates is named "Date".
  ## the column "Date" has class Date

  ## extract the year, month and day from the Date field of the dataFrame
  year <- as.numeric(format(dataFrame[,"Date"],"%Y"))
  month <- as.numeric(format(dataFrame[,"Date"],"%m"))
  day <- as.numeric(format(dataFrame[,"Date"],"%d"))
  
  ## create a data.frame with the year and month to be used with the tapply
  ## function in order to extract the end/start of month date
  index <- data.frame(year,month)
  
  ## create a matrix "Day" where the rows are the years and the columns the month
  ## such that the value in "Day[year,month]" is the corresponding end/start of month day
  if (endOfMonth)
  {
    Day <- tapply(day,INDEX=index,FUN=max)
  } else {
    Day <- tapply(day,INDEX=index,FUN=min)  
  }
  year <- dimnames(Day)[[1]]
  month <- completeDatePart(dimnames(Day)[[2]])

  ## construct the vector of end/start of month dates
  Year <- rep(year,each=length(month))
  Month <- rep(month,length(year))
  
  tmp1 = rep(1:length(year),each=length(month))
  tmp2 = rep(1:length(month),length(year))
  i = matrix(c(tmp1,tmp2),ncol=2)
  yearMonth <- matrix(c(Year,Month),ncol=2)
  isOk <- !is.na(Day[i])
  v.endStartMonth <- completeDatePart(as.character(Day[i][isOk]))
  datesToExtract <- paste(yearMonth[isOk,1],yearMonth[isOk,2],v.endStartMonth,sep="-")
  return(datesToExtract)
}

df_dailyToMonthly <- function(dataFrame,datiMensili)
{
  ## dataFrame contiene la prima colonna di date (formato data) seguita dalle colonne di dati
  ## this function transform a daily dataFrame into a list with the daily end of month dates
  ## and corresponding observations
  
  dateDatiGiornalieri <- substr(as.character(dataFrame[,"Date"]),1,10)
  toExtract <- is.element(dateDatiGiornalieri,dailyToMonthly(dataFrame)) 
  datiGiornalieriMensili <- dataFrame[toExtract,,drop=FALSE]
  
  ## the monthly prices are end of month values even if the starting day is 01
  ## and therefore in order to do the correct matching we move the end of day
  ## date of the daily prices to "01"
  dateDatiGiornalieriMensili <- substr(as.character(datiGiornalieriMensili[,1]),1,10)
  dateDatiGiornalieriMensili <- paste(substr(dateDatiGiornalieriMensili,1,8),"01",sep="")
  rownames(datiGiornalieriMensili) <- dateDatiGiornalieriMensili
 
  
  DateDatiMensili <- substr(as.character(datiMensili[,1]),1,10)
  rownames(datiMensili) <- DateDatiMensili
  DatePerCalcolo <- intersect(dateDatiGiornalieriMensili, DateDatiMensili)
  
  usa1 <- dateDatiGiornalieriMensili[is.element(dateDatiGiornalieriMensili,DatePerCalcolo)]
  usa2 <- DateDatiMensili[is.element(DateDatiMensili,DatePerCalcolo)]
  
  DatiPerCalcolo <- cbind(datiGiornalieriMensili[usa1,,drop=FALSE],
                          datiMensili[usa1,-1,drop=FALSE])
  Z <- list()
  Z$Dati <- DatiPerCalcolo[,-1,drop=FALSE]
  Z$Date <- DatiPerCalcolo[,1,drop=FALSE]

  return(Z)
}

subjectiveCov <- function(logReturns,conversion,stdevSoggettiveFreqOsservaz,
                          v.stdevSoggettive)
{
  ## logReturns: data.frame of logReturns
  ## conversion: integer used to convert the historical variance to a yearly base
  ## stdevSoggettiveFreqOsservaz: frequency of the subjective stdev 
  ## v.stdevSoggettive: the values of the subjective stdev
  ## nomiStdevSoggettive: the names of the time series for which to modify the variance

  Z <- list()
  nrSerie = ncol(logReturns)
  nomiStdevSoggettive <- names(v.stdevSoggettive)
  v.nomiSerieStoriche <- colnames(logReturns)
  
  conversionSoggettive = switch(stdevSoggettiveFreqOsservaz, 252, 52, 12, 1)
  

  ## verifica se ci sono varianze soggettive che non hanno la serie storica
  ## corrispondente
  assenti <- !is.element(nomiStdevSoggettive,v.nomiSerieStoriche)
  if (any(assenti))
    {
      string = "Ci sono varianze soggettive senza una serie storica corrispondente:\n"
      tmp <- paste(nomiStdevSoggettive[assenti],collapse=", ")
      string = paste(string, tmp, sep="")
      tkmessageBox(message=string,icon="error")
      exitWithError <<- 1
      errorString <<- "Ci sono varianze soggettive senza una serie storica corrispondente"
      save.image("error.RData")
      options(error=expression(NULL))      
    }
    
  ## standardizza i logReturns
  X <- standardize(logReturns)
  X$standardizedData
  
  
  ## annualizza le deviazioni standard (il risultato deve essere la stdev annualizzata,
  ## sia per le stdev storiche che per quelle soggettive
  v.stdevDesiderate <- sqrt(X$v.variances) * sqrt(conversion)
  v.stdevDesiderate[nomiStdevSoggettive] <- v.stdevSoggettive * sqrt(conversionSoggettive)
  
  ## moltiplica i logReturns standardizzati per le devStandard desiderate
  logReturns = X$standardizedData
  for (Name in v.nomiSerieStoriche)
    {
      logReturns[,Name] = logReturns[,Name] * v.stdevDesiderate[Name] 
    }
  
  
  Z$averageLogReturns <- X$v.averages
  Z$standardizedLogReturns <- X$standardizedData
  Z$rho <- cor(logReturns,use="pair")
  Z$S <- cov(logReturns,use="pair")
  return(Z)
}


################################################################################
##################            START PROCEDURE              #####################
################################################################################


## transform some variables coming from MS Excel
## rename the data.frame columns. The first column must be the unique column of
## dates



if (exists("nuoviNomiTimeSeries_df")) {
   colnames(nuoviNomiTimeSeries_df) <- c("Old names","New names")
   v.nomiSerieStoriche <- cambiaNomi(v.nomiSerieStoriche,nuoviNomiTimeSeries_df)
   rm(nuoviNomiTimeSeries_df)   
}

v.nomiSerieStoriche <- trim(v.nomiSerieStoriche[-(1:3)]) # skip the "Month", "Day" and "Date" 
colnames(dati_df) <- c("Year","Month","Day","Date",v.nomiSerieStoriche)

date.v <- paste(dati_df[,"Year"],completeDatePart(dati_df[,"Month"]),
                completeDatePart(dati_df[,"Day"]),sep="-")

df.tmp <- dati_df[,-(1:4),drop=FALSE]
dati_df <- data.frame("Date"=date.v,df.tmp)
colnames(dati_df) = c("Date",v.nomiSerieStoriche) # not the "Month" and "Day" column
l.timeSeries <- toTimeSeries(dati_df)
rm(date.v,tmp.df)


## verify the strict positivity of prices
isProblem <- sapply(l.timeSeries,verifyPositivity)                 
if (any(isProblem)) {
	string = "Some timeseries have zero or negative values!"
	tkmessageBox(message=string,icon="error")
	exitWithError = 1
	save.image("error.RData")
	stop()
}


## clean the names of the subjective standard deviations if available
if (exists("df_stdevSoggettive",inherits = FALSE))
{

  ## count the number of subjective standard deviations. If this number is 0 
  ## show a warning and then exit the procedure.
  if (ncol(df_stdevSoggettive)==0)
  {
    string = "The number of subjective standard deviations is zero! "
    tkmessageBox(message=string,icon="error")
    exitWithError = 1
    save.image("error.RData")
    stop()   
  }
  
  nomiStdevSoggettive <- trimString(as.character(df_stdevSoggettive[,"Nome"]))
  v.stdevSoggettive <- as.numeric(df_stdevSoggettive[,"Stdev"])
  names(v.stdevSoggettive) <- nomiStdevSoggettive
  conStdevSoggettive = TRUE
} else {
  conStdevSoggettive = FALSE
}


## create the list where to store the original prices
P <- list()
P$l.originalTimeSeries <- l.timeSeries

# execute a linear interpolation of missing values
lapply(l.timeSeries,linearInterpolate)
P$l.timeSeries <- l.timeSeries
P$l.logReturns <- lapply(l.timeSeries,computeLogReturns)

## create the list containing the data (prices and returns at different freq.)
Z <- list()

## extract the prices at the desired frequency
if (frequenzaCalcolo < frequenzaOsservazioni)
{
  string = "The computation frequency is too high with respect to the data frequency."
  tkmessageBox(message=string,icon="error")
  exitWithError = 1
  save.image("error.RData")
  stop() 
}

############################## Step 1: A dailyNoWeekend frequency
if (frequenzaOsservazioni == 1) ## dailyNoWeekend
{
  ## da completare: mettere un controllo che non ci siano buchi infrasettimanali.
  ## Date <- as.Date(dati_df[,"Date"]) 
  if (frequenzaCalcolo == 1)
  {
    Z$Date <- dati_df[,1,drop=FALSE]
    Z$dati_df <- dati_df[,-1,drop=FALSE]
  }
  
  if (frequenzaCalcolo == 2) # Weekly at specified date
  {
    
	  
	Day = as.numeric(format(dati_df[,"Date"],"%w"))
	
    isDesiredDay = Day == frequenzaCalcoloGiorno
    nbObs <- length(isDesiredDay)
	extractedDates <- 1:nbObs[isDesiredDay]
	dayDiff <- extractedDates[-1] - extractedDates[-length(extractedDates)]
	
	if (!all(isDesiredDay))
    {
      string = "Daily data are not complete.\nSome desired day is missing."
      tkmessageBox(message=string,icon="error")
      exitWithError = 1
      save.image("error.RData")
      stop()    
    }
    Z$dati_df <- dati_df[isDesiredDay,-1,drop=FALSE]
    Z$Date <- dati_df[isDesiredDay,1,drop=FALSE]
  }
  
  if (frequenzaCalcolo > 3)
  {
    string = "La trasformazione da frequenza giornaliera a frequenza annua non � ancora implementata"
    tkmessageBox(message=string,icon="error")
    exitWithError = 1
    save.image("error.RData")
    stop()   
  }
}

############################## Step 1: B weekly frequency
if (frequenzaOsservazioni == 2) ## settimanale
{
   
  if (frequenzaCalcolo == 2)
  {
    ## da completare mettere il controllo che sia effettivamente il giorno desiderato
    Day = as.numeric(format(dati_df[,"Date"],"%w"))
    isDesiredDay = Day == frequenzaCalcoloGiorno
    if (!all(isDesiredDay))
    {
      string = "Le date a frequenza settimanale non contengono il giorno \ndesiderato per tutte le osservazioni!"
      tkmessageBox(message=string,icon="error")
      exitWithError = 1
      save.image("error.RData")
      stop()     
    }
    
    Z$dati_df <- dati_df[,-1,drop=FALSE]
    Z$Date <- dati_df[,1,drop=FALSE]  
  }
  if (frequenzaCalcolo > 2)
  {
    string = "At the moment the computation at a monthly or higher frequency is not implemented."
    tkmessageBox(message=string,icon="error")
    exitWithError = 1
    save.image("error.RData")
    stop()    
  }
}
############################ Step 1: C other frequency
if (frequenzaOsservazioni > 2)
{
  Z$dati_df <- dati_df[,-1,drop=FALSE]
  Z$Date <- dati_df[,1,drop=FALSE] 
}

############################## Step 1: D compute the logReturns
## compute the log price
Z$ldati <- log(as.matrix(Z$dati_df))

############################## Step 1: D1 verify some conditions 
Z$nrSerie = ncol(Z$ldati)

if (Z$nrSerie < 1) 
{
  string = "The number of time series is zero."
  tkmessageBox(message=string,icon="error")
  exitWithError = 1
  save.image("error.RData")
  stop()
}

# compute the number of observations
Z$nrObs = nrow(Z$ldati)
# if the number of observation is smaller than 3 show a warning and exit.
if (Z$nrObs <= 2) 
{
  string = "The number of available observation is less than 3."
  tkmessageBox(message=string,icon="error")
  exitWithError = 1
  save.image("error.RData")
  stop()
}

if (Z$nrObs <= 20) 
{
  string = paste("The number of observations is very low:",Z$nrObs)
  tkmessageBox(message=string,icon="error")
}


if (Z$nrObs <= Z$nrSerie) 
{
  string = "The number of observation is less or equal to the number of time series.\nInstability/infeasibility problems are possible."
  tkmessageBox(message=string,icon="error")
}


############################## Step 1: D2 compute the log returns 
# calcola i rendimenti logaritmici e rimuovi la serie dei prezzi logaritmici
Z$logReturns <- Z$ldati[-1,,drop=FALSE]-Z$ldati[-Z$nrObs,,drop=FALSE]
Z$ldati <- NULL


############################## Step 1: E adjust the variances if necessary and compute the cor & cov
conversion = switch(frequenzaCalcolo, 252, 52, 12, 1)

if (!conStdevSoggettive)
{
  # compute the historical returns, volatilities, the correlation matrix and the covariance matrix  
  Z$historicalAverageLogReturns <- mean(Z$logReturns)
  Z$rho <- cor(Z$logReturns,use="pair")
  Z$S <- cov(Z$logReturns,use="pair") * conversion
} else {
  nomi <- nomiStdevSoggettive
  if(conSerieMensili) nomi <- setdiff(nomi, v.nomiSerieStoricheMensili)
  if (length(nomi)> 0)
    {
      V <- subjectiveCov(as.matrix(Z$logReturns),conversion,stdevSoggettiveFreqOsservaz,v.stdevSoggettive[nomi])
      ## copy the list V into the list Z
      Z = c(Z,V)
      rm(V,nome)
    } else { ## in this case al subjective series are monthly. Proceed as if there were no subjective series
      # compute the historical returns, volatilities, the correlation matrix and the covariance matrix  
      Z$historicalAverageLogReturns <- mean(Z$logReturns)
      Z$rho <- cor(Z$logReturns,use="pair")
      Z$S <- cov(Z$logReturns,use="pair") * conversion    
    }
      
} ## fine  "if (!conStdevSoggettive)"

############################## Step 1: F write the available time series, i.e. those with a corr/covariance matrix 
Z$serieDisponibili <- matrix(rownames(Z$S),ncol=1)
Z$Stdev <- matrix(sqrt(diag(Z$S)),ncol=1)
dimnames(Z$Stdev) <- list(Z$serieDisponibili,"Stdev")


###################         END STEP 1                    ######################
################################################################################


if (exitWithError!=1) save.image("cov_matrix_output.RData")