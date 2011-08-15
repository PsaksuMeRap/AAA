# TODO: Add comment
# 
# Author: claudio
###############################################################################


copyPosition <- function(position) UseMethod("copyPosition")


copyPosition.default <- function(position) {
	stop(paste("No suitable method for copyPosition of class",class(position)))
}


copyPosition.position <- function(position) {
	
	newPosition <- create_position()
	newPosition$create(name=position$name,
			currency=position$money$currency,
			amount=position$money$amount)
	class(newPosition) <- class(position)
	return(newPosition)
}

copyPosition.Fondi_obbligazionari <- function(position,...) {
	newPosition <- copyPosition.position(position)
	
	newPosition$numeroValore <- position$numeroValore 
	return(newPosition)
}

copyPosition.Fondi_azionari <- function(position,...) {
	newPosition <- copyPosition.position(position)
	
	newPosition$numeroValore <- position$numeroValore 
	return(newPosition)
}



copyPosition.Fondi_misti <- function(position) {
	newPosition <- copyPosition.position(position)
	
	newPosition$quotaEquities <- position$quotaEquities
	newPosition$quotaBonds <- position$quotaBonds
	return(newPosition)
}


copyPosition.FX_Forward <- function(position) {
	newPosition <- copyPosition.position(postition)

	newPosition$expiry <- position$expiry
	return(newPosition)
}


copyPosition.equity <- function(position) {
	newPosition <- copyPosition.position(position)
	
	# create the id field and identify the ticker of the equity
	newPosition$id <- position$id
	
	newPosition$ticker <- position$ticker	
	
	# store the numeroValore
	newPosition$numeroValore <- position$numeroValore
	
	newPosition$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		
		fields <- newPosition$fieldsToPrintDefault(width)
		fields$ticker <- newPosition$ticker
		fields$ID_AAA <- newPosition$ID_AAA
		return(fields)
	}
	
	return(newPosition)
}

copyPosition.Strutturati_FI <- function(position) {
	newPosition <- copyPosition.position(position)
	
	# check if it is a short term fixed income position
	newPosition$underlyingHorizon <- position$underlyingHorizon
	newPosition$expiryDate	<- position$expiryDate
	
	return(newPosition)
}

copyPosition.bond <- function(position) {
	newPosition <- copyPosition.position(position)
	
	newPosition$getMaturity <- function() {
		# extract the maturity
		name <- newPosition$name	
		paymentDate <- substr(name,nchar(name)-8+1,
				nchar(name))
		
		# verifica che il nome sia una data
		day <- substr(paymentDate,1,2)
		month <- substr(paymentDate,4,5)	
		year <- paste("20",substr(paymentDate,7,8),sep="")		
		paymentDate <- paste(year,month,day,sep="-")
		
		if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=paymentDate,perl=TRUE)) {
			message <- newPosition$toString()
			message <- paste("Invalid date parsed",paymentDate,"for position:\n",message)
			stop(message)
		}
		return(paymentDate)
	}
	return(newPosition)
}


copyPosition.accruedInterest <- function(position) {
	
	newPosition <- copyPosition.position(position)
		
	# create the accruedInterest
	newPosition$accruedInterest <- position$accruedInterest
	
	# copy the numeroValore
	newPosition$numeroValore <- position$numeroValore
	
	newPosition$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		
		fields <- newPosition$fieldsToPrintDefault(width)
		
		fields$accruedInterest <- "Accrued interest"
		
		return(fields)
	}
	
	return(newPosition)
}


weightPosition <- function(position,weight) {
	position$money$amount <- weight * position$money$amount
	#position$origin$Saldo <- weight * position$origin$Saldo
	#position$origin$ValorePosizione <- weight * position$origin$ValorePosizione			
	#position$origin$ValoreMonetaRiferimento <- weight * position$origin$ValoreMonetaRiferimento	
	#position$origin$ValoreMercatoMonetaCHF <- weight * position$origin$ValoreMercatoMonetaCHF						
	#position$origin$ValoreMercatoMonetaEUR <- weight * position$origin$ValoreMercatoMonetaEUR					
	#position$origin$ValoreMercatoMonetaUSD <- weight * position$origin$ValoreMercatoMonetaUSD
}