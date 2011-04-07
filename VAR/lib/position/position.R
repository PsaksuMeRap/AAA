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
#	position$amount = NA_real_
	position$money = NA
#	position$money$currency = NA_character_
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0,origin=NA) {
		position$name <<- name
#		position$amount <<- amount
		position$money <<- toMoney(amount,currency)
#		position$money$currency <<- currency
		position$origin <<- origin
	}
	
	
	position$isInstrument <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$isCurrency <- function(currency) {
		return(position$money$currency==currency)
	}
	
	position$print <- function(width) {
		if (missing(width)) {
			x <- formatC(position$money$amount,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		} else {
			x <- formatC(position$money$amount,width=width,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		}
		print(paste(class(position)[1],"/",position$money$currency,
						x,
						"/ Name:", position$name
				)
		)
	}
	
	position$toString <- function(width) {
		if (missing(width)) {
			x <- formatC(position$money$amount,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		} else {
			x <- formatC(position$money$amount,width=width,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		}
		
		string <- paste(class(position)[1],"/",position$money$currency,
				x,
#				formatC(position$amount,digits=2,format="f"),
				"/ Name:", position$name)
		return(string)
	}
	
	position$toDataFrame <- function() {
		# this function create a data.frame from the list of positions
		df <- data.frame(instrument=class(position)[1],
				name=position$name,
				currency=position$money$currency,
				amount=position$money$amount,stringsAsFactors=FALSE
				)
		return(df)
	}
	return(position)
}

