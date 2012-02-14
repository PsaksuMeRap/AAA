# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Money/Money.R")
source("./lib/classDefinitions/Quantity/Quantity.R")
source("./lib/classDefinitions/Security/Security.R")

# crea la classe virtuale "Position"
setClass("Position",representation(id="Id",security="Security",quantity="Quantity",value="Money"))


# crea il metodo che formatta e raccoglie i campi da stampare

setGeneric("fieldsToPrint",def=function(position,width) standardGeneric("fieldsToPrint"))

setMethod("fieldsToPrint",signature(position="Position",width="list"),
		function(position,width) {
			# this function returns a named list with the formatted strings
			fields <- list()
			
			fields$class <- is(position)[1]
			fields$currency <- position@money@currency
			
			# format the class name
			if (is.element("className",names(width))) {
				nbChar <- nchar(fields$class)
				fields$class <- paste(fields$class,paste(rep(" ", width["className"] - nbChar),collapse=""),sep="")
			}
			
			if (is.element("amount",names(width))) {	
				fields$amount <- formatC(position@money@amount,width=width[["amount"]])
			} else {
				fields$amount <- formatC(position@money@amount)
			}
			fields$name <- position@security@name
			
			# il codice sottostante non riguarda la posizione!
			#if (exists("explodeString",envir=position)) {
			#	fields$explodeString <- position$explodeString
			#}
			
			return(fields)	
			
		}
)

fieldsToPrint <- function(width) {
	
	if (missing(width)) width=c(empty=TRUE)
	fields <- position$fieldsToPrintDefault(width)
	
	return(fields)
}





setMethod("as.character","Position", function(width) {
			
			if (missing(width)) width=c(empty=TRUE)
			
			f <- position$fieldsToPrint(width)
			
			string <- paste(f,collapse=" / ")
			
			return(string)
		}
)
