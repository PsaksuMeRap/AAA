# TODO: Add comment
# 
# Author: claudio
###############################################################################


extendPosition <- function(position) UseMethod("extendPosition",position)

extendPosition.default <- function(position) {
	
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

extendPosition.bond <- function(position) {
	
	if (position$origin["Strumento"]=="Ooacc") {
		position$accruedInterest <- position$amount
		
		nameLength <- length(position$name)
		# " Pro-rata" must be removed from the name
		position$name <- substr(position$name,1,nameLength-9)
		# extract the date of the payment
		date <- substr(position$name,nameLength-8-9,nameLength-9)
		date <- paste("20",date,sep="")		
		# create the accruedInterest
		money <- toMoney(position$amount,position$currency)
		position$accruedInterest <- create_accruedInterest(money,date)
	} else {
		position$accruedInterest <- NA
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
