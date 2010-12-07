# TODO: Add comment
# 
# Author: claudio
###############################################################################


extendPosition <- function(position) UseMethod("extendPosition",position)

extendPosition.default <- function(position) {
	if (is.element("Strutturati FI",class(position))) {
		extendPosition.StrutturatiFI(position)
	}
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
	
	position$ticker <- repositories$equities$tickerFromId(id)
	position$print <- function()
	{
		print(paste(class(position)[1],"/",position$currency, "-", position$amount,
						"/ Name:", position$name,
						"/ Ticker:", position$ticker
				)
		)
	}
	return()
}

extendPosition.StrutturatiFI <- function(position) {
	name <- position$name
	
	# check if it is a short term fixed income position
	if (grepl("<3Y",x=name)) position$underlyingHorizon = "<3Y"
	if (grepl(">3Y",x=name)) position$underlyingHorizon = ">3Y"
	
	year <- substr(name,1,4)
	month <- substr(name,5,6)
	day <- substr(name,7,8)
	position$expiryDate = paste(year,month,day,sep="-")
	
	position$print <- function()
	{
		print(paste(class(position)[1],"/",position$currency, "-", position$amount,
						"/ Name:", position$name
				)
		)
	}
	return()
	
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
		money <- toMoney(position$amount,position$currency)
		assign("accruedInterest",create_accruedInterest(money,paymentDate),envir=position,inherits=FALSE)
	}
	
	position$print <- function()
	{
		print(paste(class(position)[1],"/",position$currency, "-", position$amount,
						"/ Name:", position$name,
						"/ Accrued interest"
				)
		)
	}
	return()
}
