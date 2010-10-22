# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")
source("./lib/money.R")



create_position <- function() {
	position <- new.env() # list()
	class(position) <- "position"
	
	position$name = NA_character_
	position$currency = NA_character_
	position$amount = NA_real_
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0,origin=NA) {
		position$name <<- name
		position$currency <<- currency
		position$amount <<- amount
		position$origin <<- origin
	}
	
	
	position$isInstrument <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$isCurrency <- function(currency) {
		return(position$currency==currency)
	}
	
	position$extendEquities <- function() {
		# create the repository of the equities if not available
		
		if (!exists("equities",envir=repositories,inherits=FALSE)) {
			eval(expression(equities <- create_repositoryEquities())
					,env=repositories)
		}
		
		# identify the ticker of the equity
		id <- position$origin[["ID_AAA"]]
		if (is.na(id)) {print("Errore: azione senza ID_AAA"); stop()}
		id <- as.numeric(id)
		
		position$ticker <<- repositories$equities$tickerFromId(id)
		position$print <<- function()
		{
			print(paste(class(position)[1],"/",position$currency, "-", position$amount,
							"/ Name:", position$name,
							"/ Ticker:", position$ticker
						)
					)
		}
		return()		
		
	}
	
	position$print <- function() {
		print(paste(class(position)[1],"/",position$currency,"-", position$amount, "/ Name:", position$name))
	}
	
	#elimina questa funzione se non serve
	# position$class <- function() return(class((position)))
	
	position$toDataFrame <- function() {
		# this function create a data.frame from the list of positions
		df <- data.frame(instrument=class(position)[1],
				name=position$name,
				currency=position$currency,
				amount=position$amount,stringsAsFactors=FALSE
				)
		return(df)
	}
	return(position)
}

