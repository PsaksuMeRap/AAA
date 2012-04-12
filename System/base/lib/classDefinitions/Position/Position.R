# TODO: Add comment
# 
# Author: claudio
###############################################################################

#source("./lib/classDefinitions/Money/Money.R")
#source("./lib/classDefinitions/Quantity/Quantity.R")
#source("./lib/classDefinitions/Security/Security.R")

# crea la classe virtuale "Position"
setClass("Position",representation(id="Id",security="Security",
				quantity="Quantity",value="Money"))


# crea il metodo che formatta e raccoglie i campi da stampare

setGeneric("fieldsToPrint",def=function(position,width) standardGeneric("fieldsToPrint"))

setMethod("fieldsToPrint",signature(position="Position"),
		function(position,width=list(empty=TRUE)) {
			# this function returns a named list with the formatted strings
			fields <- list()
			
			fields$securityClassName <- is(position@security)[1]
			fields$currency <- as.character(position@value@currency)
			
			# format the security class name
			if (is.element("securityClassName",names(width))) {
				nbChar <- nchar(fields$securityClassName)
				fields$securityClassName <- paste(fields$securityClassName,paste(rep(" ", width[["securityClassName"]] - nbChar),collapse=""),sep="")
			}
			
			if (is.element("amount",names(width))) {	
				fields$amount <- formatC(position@value@amount,width=width[["amount"]])
			} else {
				fields$amount <- formatC(position@value@amount)
			}
			fields$name <- position@security@name
			
			# il codice sottostante non riguarda la posizione!
			#if (exists("explodeString",envir=position)) {
			#	fields$explodeString <- position$explodeString
			#}
			
			return(fields)	
			
		}
)


setMethod("as.character","Position", 
		function(x,width=list(empty=TRUE)) {	
			if (missing(width)) width=list(empty=TRUE)
			
			f <- fieldsToPrint(x,width)
			
			string <- paste(f,collapse=" / ")
			
			return(string)
		}
)


setMethod("print","Position",
		function(x,width=list(empty=TRUE)) {
			print(as.character(x,width=width))
		}
)


setMethod("/",signature(e1="Position",e2="Money"),
		function(e1,e2) {
			if (e2@amount==0) {
				if (e1@value@amount==0) return(NaN)
				if (e1@value@amount<0) return(-Inf)
				return(Inf)	
			}
			if (identical(e2@currency,e1@value@currency)) return(unclass(e1@value@amount/e2@amount))
			e2 <- repositories$exchangeRates$exchange(e2,e1@value@currency)
			return(unclass(e1@value@amount/e2@amount))
		}
)

setGeneric("reweight",def=function(x,...) standardGeneric("reweight"))

setMethod("reweight",signature(x="Position"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			return(position)
		}
)
