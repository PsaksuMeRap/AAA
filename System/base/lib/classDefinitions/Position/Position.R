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


# crea il metodo che formatta e raccoglie i campi da stampare
setGeneric("fieldsAsCharacter",def=function(x,...) standardGeneric("fieldsAsCharacter"))

setMethod("fieldsAsCharacter",signature(x="Position"),
		function(x,referenceCurrency) {
			# this function returns a named vector with the fields converted to strings
			fields <- vector(mode="character",length = 4)
			names(fields) <- c("securityClassName","currency","amount","securityName")
			
			fields["securityClassName"] <- is(x@security)[1]
			fields["currency"] <- as.character(x@value@currency)
			fields["amount"] <- formatC(x@value@amount)
			if (.hasSlot(x,"rating")) {
				fields["securityName"] <- paste(x@security@name,x@rating,sep=" rated: ")
			} else {
				fields["securityName"] <- x@security@name
			}
			if (!missing(referenceCurrency)) {
				fields["referenceCurrency"] <- as.character(referenceCurrency)
				fields["referenceCurrencyAmount"] <- formatC(exchange(x@value,referenceCurrency)@amount)
			}
			return(fields)
		}
)

setMethod("groupBySecurityId",signature(x="Position",y="Position"),
		function(x,y) {
			
			classOfx <- class(x)[[1]]
			z <- new(classOfx,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)

#setGeneric("fieldsToPrint",def=function(position,width,...) standardGeneric("fieldsToPrint"))

#setMethod("fieldsToPrint",signature(position="Position"),
#		function(position,width=list(empty=TRUE),referenceCurrency) {
#			# this function returns a named list with the formatted strings
#			fields <- list()
#			
#			fields$securityClassName <- is(position@security)[1]
#			fields$currency <- as.character(position@value@currency)
#			
#			# format the security class name
#			if (is.element("securityClassName",names(width))) {
#				nbChar <- nchar(fields$securityClassName)
#				fields$securityClassName <- paste(fields$securityClassName,paste(rep(" ", width[["securityClassName"]] - nbChar),collapse=""),sep="")
#			}
#			
#			if (is.element("amount",names(width))) {	
#				fields$amount <- formatC(position@value@amount,width=width[["amount"]])
#			} else {
#				fields$amount <- formatC(position@value@amount)
#			}
#			if (!missing(referenceCurrency)) {
#				fields$referenceCurrency <- referenceCurrency
#				fields$amountInReferencecurrency <- formatC(exchange(position@value,referenceCurrency)@amount)
#			}
#			fields$name <- position@security@name
#			
#			return(fields)	
#			
#		}
#)


setMethod("as.character","Position", 
		function(x,referenceCurrency) {	
			if (missing(referenceCurrency)) {
				f <- fieldsAsCharacter(x)				
			} else {
				f <- fieldsAsCharacter(x,referenceCurrency)					
			}
			
			string <- paste(f,collapse=" / ")
			
			return(string)
		}
)


setMethod("print","Position",
		function(x,referenceCurrency) {
			if (missing(referenceCurrency)) {
				print(as.character(x))
			} else {
				print(as.character(x),referenceCurrency)
			}
		}
)