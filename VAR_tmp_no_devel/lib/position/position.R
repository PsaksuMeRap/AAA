# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")
source("./lib/money.R")
source("./lib/position/extendPosition.R")


create_position <- function() {
	position <- list()
	class(position) <- "position"
	
	position$name = NA_character_
	position$money = toMoney()
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0) {
		position$name <<- name
		position$money <<- toMoney(amount,currency)
	}
		
	position$isInstrument <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$isCurrency <- function(currency) {
		return(position$money$currency==currency)
	}
	
	position$fieldsToPrintDefault <- function(width) {
		
		fields <- list()
		
		fields$class <- class(position)[1]
		fields$currency <- position$money$currency
		
		# format the class name
		if (is.element("className",names(width))) {
			nbChar <- nchar(fields$class)
			fields$class <- paste(fields$class,paste(rep(" ", width["className"] - nbChar),collapse=""),sep="")
		}
		
		if (is.element("amount",names(width))) {	
			fields$amount <- formatC(position$money$amount,width=width[["amount"]],big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		} else {
			fields$amount <- formatC(position$money$amount,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
		}
		fields$name <- position$name

		if (exists("explodeString",envir=position)) {
			fields$explodeString <- position$explodeString
		}
		return(fields)	
	}
	
	position$fieldsToPrint <- function(width) {
		
		if (missing(width)) width=c(empty=TRUE)
		fields <- position$fieldsToPrintDefault(width)
		
		return(fields)
	}
	
	position$toString <- function(width) {
	
		if (missing(width)) width=c(empty=TRUE)
		
		f <- position$fieldsToPrint(width)
		
		string <- paste(f,collapse=" / ")
		
		return(string)
	}
	
	position$print <- function(width) {
		if (missing(width)) width=c(empty=TRUE)
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
	
	position$isConsistent <- function() {
		# this function check for the consistency of the fields
		# at the moment it checks for NA values only
		
		isNotAvailable <- c(name=is.na(position$name),
				amount=is.na(position$money$amount),
				currency=is.na(position$money$currency)
		)
		
		if (any(isNotAvailable)) return(FALSE) else return(TRUE)
		
	}
	
	return(position)
}

