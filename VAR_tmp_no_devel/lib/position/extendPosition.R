# TODO: Add comment
# 
# Author: claudio
###############################################################################

# attenzione: modifiche nei campi di extendPosition devono essere rispecchiate
# in positionMethods.R copyPosition

extendPosition <- function(position,...) UseMethod("extendPosition")

extendPosition.default <- function(position,...) {
	stop(paste("No suitable method for extendPosition of class",class(position)))
}

extendPosition.position <- function(position,...) {
	
}


extendPosition.Fondi_obbligazionari <- function(position,...) {
	origin <- list(...)[[1]]
	
	position$id <- origin[["ID_AAA"]]
	
	return(position)
}

extendPosition.Fondi_azionari <- function(position,...) {
	origin <- list(...)[[1]]
	
	position$id <- origin[["ID_AAA"]]
	
	return(position)
}

extendPosition.Fondi_misti <- function(position,...) {
	origin <- list(...)[[1]]
	
	position$id <- origin[["ID_AAA"]]
	
	split1 <- strsplit(position$name," ")
	split2 <- unlist(strsplit(split1[[1]][1],"-"))
	errorMessage <- paste("Errore nel parsare fondo misto:",position$name,"\nMancano i pesi %!")
	
	if (length(split2)!=2) stop(errorMessage)
	
	if (all(grepl("^[0-9]+.*[0-9]*$",split2))) {
		position$quotaBonds <- as.numeric(split2[1])
		position$quotaEquities <- as.numeric(split2[2])
	} else {
		stop(errorMessage)
	}
	return(position)
}

extendPosition.FX_Forward <- function(position,...) {

	parseDate <- function(position) {
		name <- position$name
		nameLength <- nchar(name)
		if (nameLength < 10) stop(paste("Error: name of FX_Forward",name,"wrong!"))
		dateString <- substr(name,nameLength-10+1,nameLength)
		day <- substr(dateString,1,2)
		month <- substr(dateString,4,5)
		year <- substr(dateString,7,10)
		return(paste(year,month,day,sep="-"))
	}
	position$expiry <- parseDate(position)
	return(position)
}

extendPosition.equity <- function(position,...) {

	origin <- list(...)[[1]]
	if (length(origin)==0) stop(paste("Equity",position$name,"without origin"))
	
	# create the repository of the equities if not available	
	if (!exists("equities",envir=repositories,inherits=FALSE)) {
		eval(expression(equities <- create_repositoryEquities())
				,env=repositories)
	}

	# create the id field and identify the ticker of the equity
	id <- origin[["ID_AAA"]]
	if (is.null(id)) stop(paste("Call to extendPosition.equity for",position$name,"without ID_AAA argument"))
	if (is.na(id)) stop(paste("Equity",position$name,"with ID_AAA = NA"))
	id <- as.numeric(id)
	position$id <- id
	
	position$ticker <- unique(repositories$equities$tickerFromId(id)) 
	
	
	position$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		
		fields <- position$fieldsToPrintDefault(width)
		fields$ticker <- position$ticker
		fields$id <- position$ID_AAA
		return(fields)
	}

	return(position)
}

extendPosition.Strutturati_FI <- function(position,...) {
	origin <- list(...)[[1]]
	
	position$id <- origin[["ID_AAA"]]
	
	name <- position$name
	
	# check if it is a short term fixed income position
	if (grepl("<3Y",x=name)) position$underlyingHorizon = "<3Y"
	if (grepl(">3Y",x=name)) position$underlyingHorizon = ">3Y"
	
	year <- substr(name,1,4)
	month <- substr(name,5,6)
	day <- substr(name,7,8)
	position$expiryDate = paste(year,month,day,sep="-")
	
	return(position)
	
}

extendPosition.bond <- function(position,...) {
	origin <- list(...)[[1]]
	
	position$id <- origin[["ID_AAA"]]
	
	position$getMaturity <- function() {
		# extract the maturity
		name <- position$name	
		paymentDate <- substr(name,nchar(name)-8+1,
				nchar(name))
		
		# verifica che il nome sia una data
		day <- substr(paymentDate,1,2)
		month <- substr(paymentDate,4,5)	
		year <- paste("20",substr(paymentDate,7,8),sep="")		
		paymentDate <- paste(year,month,day,sep="-")
		
		if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=paymentDate,perl=TRUE)) {
			message <- position$toString()
			message <- paste("Invalid date parsed",paymentDate,"for position:\n",message)
			stop(message)
		}
		return(paymentDate)
	}
	return(position)
}


extendPosition.accruedInterest <- function(position,...) {
	origin <- list(...)[[1]]
	
	# " Pro-rata" must be removed from the name
browser()	
	nameLength <- nchar(position$name)
	position$name <- substr(position$name,1,nameLength-9)
	
	# extract the date of the payment
	nameLength <- nchar(position$name)
	paymentDate <- substr(position$name,nameLength-7,nameLength)
	
	position$name <- substr(position$name,1,nameLength-9)
	day <- substr(paymentDate,1,2)
	month <- substr(paymentDate,4,5)
	year <- substr(paymentDate,7,8)
	paymentDate <- paste("20",year,"-",month,"-",day,sep="")		
	
	# create the accruedInterest
	money <- toMoney(position$money$amount,position$money$currency)
	#assign("accruedInterest",create_accruedInterest(money,paymentDate),envir=position,inherits=FALSE)
	position$accruedInterest <- create_accruedInterest(money,paymentDate)
	
	# create the id field
	position$id <- origin[["ID_AAA"]]
	
	position$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		
		fields <- position$fieldsToPrintDefault(width)
		
		fields$accruedInterest <- "Accrued interest"
		
		return(fields)
	}
	
	return(position)
}

