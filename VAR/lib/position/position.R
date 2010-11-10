# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")
source("./lib/money.R")
source("./lib/position/extendPosition.R")


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
	
	position$print <- function() {
		print(paste(class(position)[1],"/",position$currency,
						formatC(position$amount,digits=2,format="f"),
						"/ Name:", position$name
				)
		)
	}
	
	position$toString <- function() {
		string <- paste(class(position)[1],"/",position$currency,
				formatC(position$amount,digits=2,format="f"),
				"/ Name:", position$name)
		return(string)
	}
	
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

