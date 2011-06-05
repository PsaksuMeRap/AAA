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
	position$money = NA
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0,origin=NA) {
		position$name <<- name
		position$money <<- toMoney(amount,currency)
		position$origin <<- origin
	}
	
	
	position$isInstrument <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$isCurrency <- function(currency) {
		return(position$money$currency==currency)
	}
	
	position$toStringDefault <- function(width) {
		fields <- list()

		fields$class <- class(position)[1]
		fields$name <- position$name
		
		# format the class name
		if (exists("className",where=width)) {
			nbChar <- nchar(fields$class)
			fields$class <- paste(fields$class,paste(rep(" ", width["className"] - nbChar),collapse=""))
		}
		
		if (exists("amount",where=width)) {	
			fields$amount <- formatC(position$money$amount,width=width[["amount"]],big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		} else {
			fields$amount <- formatC(position$money$amount,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		}
		
		fields$currency <- position$money$currency
		
		return(fields)	
	}
	
	position$toString <- function(width) {
	
		if (missing(width)) width=list(empty=TRUE)
		
		f <- position$toStringDefault(width)
		
		string <- paste(f$class,"/",f$currency,
				f$amount,
				"/ Name:", f$name)
		return(string)
	}
	
	position$print <- function(width) {
		if (missing(width)) width=list()
		print(position$toString(width))
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

