# TODO: Add comment
# 
# Author: claudio
###############################################################################





## define the fieldsToPrint generic method
fun <- function(x,width) standardGeneric("fieldsToPrintDefault")
setGeneric("fieldsToPrintDefault",fun)

## define the fieldsToPrint method for Position
fun <- function(x,width) {
	## x: a position of class Position
	## width: the desired width of the fields
	fields <- list()
	
	fields$class <- class(x)[[1]]
	fields$currency <- x@money@currency
	
	# format the class name
	if (is.element("className",names(width))) {
		nbChar <- nchar(fields$class)
		fields$class <- paste(fields$class,paste(rep(" ", width["className"] - nbChar),collapse=""),sep="")
	}
	
	if (is.element("amount",names(width))) {	
		fields$amount <- formatC(x@money@amount,width=width[["amount"]],big.mark = "'",
				decimal.mark = ".",format="f",digits=2)
	} else {
		fields$amount <- formatC(x@money@amount,big.mark = "'",
				decimal.mark = ".",format="f",digits=2)
	}
	fields$name <- x@name
	
	#??
	#if (exists("explodeString",envir=position)) {
	#	fields$explodeString <- position$explodeString
	#}
	return(fields)	
}

setMethod("fieldsToPrintDefault","Position",fun)












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







