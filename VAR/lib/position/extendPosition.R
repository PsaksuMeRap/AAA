# TODO: Add comment
# 
# Author: claudio
###############################################################################


extendPosition <- function(position) UseMethod("extendPosition",position)

extendPosition.default <- function(position) {
#	if (is.element("Strutturati_FI",class(position))) {
#		extendPosition.StrutturatiFI(position)
#	}
	stop(paste("No suitable method for extendPosition of class",class(position)))
}

extendPosition.Conto_corrente <- function(position) {
	
}

extendPosition.FX_Forward <- function(position) {

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
}

extendPosition.position <- function(position) {
	
}

extendPosition.equity <- function(position) {
	# create the repository of the equities if not available
	
	if (!exists("equities",envir=repositories,inherits=FALSE)) {
		eval(expression(equities <- create_repositoryEquities())
				,env=repositories)
	}

	# identify the ticker of the equity
	id <- position$origin[["ID_AAA"]]
	if (is.na(id)) {print("Error: equity without ID_AAA"); stop()}
	id <- as.numeric(id)
	
	position$ticker <- unique(repositories$equities$tickerFromId(id))
	
	position$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		
		fields <- position$fieldsToPrintDefault(width)
		fields$ticker <- position$ticker
		
		return(fields)
	}
	
	return()
}

extendPosition.Strutturati_FI <- function(position) {
	name <- position$name
	
	# check if it is a short term fixed income position
	if (grepl("<3Y",x=name)) position$underlyingHorizon = "<3Y"
	if (grepl(">3Y",x=name)) position$underlyingHorizon = ">3Y"
	
	year <- substr(name,1,4)
	month <- substr(name,5,6)
	day <- substr(name,7,8)
	position$expiryDate = paste(year,month,day,sep="-")
	
	return()
	
}

extendPosition.bond <- function(position) {
	
	position$getMaturity <- function() {
		# extract the maturity
		name <- position$name	
		paymentDate <- substr(name,nchar(position$name)-8+1,
				nchar(position$name))
		
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
	
}


extendPosition.accruedInterest <- function(position) {
	
	if (position$origin[["Strumento"]]=="Oacc") {

		# " Pro-rata" must be removed from the name

		name <- position$name
		nameLength <- nchar(name)
		assign("name",substr(name,1,nameLength-9),position,inherits=FALSE)
		
		# extract the date of the payment
		name <- position$name	
		nameLength <- nchar(name)
		paymentDate <- substr(name,nameLength-7,nameLength)

		assign("name",substr(position$name,1,nameLength-9),envir=position,inherits=FALSE)
		day <- substr(paymentDate,1,2)
		month <- substr(paymentDate,4,5)
		year <- substr(paymentDate,7,8)
		paymentDate <- paste("20",year,"-",month,"-",day,sep="")		

		# create the accruedInterest
		money <- toMoney(position$money$amount,position$money$currency)
		assign("accruedInterest",create_accruedInterest(money,paymentDate),envir=position,inherits=FALSE)
	}
	
	position$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		
		fields <- position$fieldsToPrintDefault(width)
		
		fields$accruedInterest <- "Accrued interest"
		
		return(fields)
	}
	
	return()
}

