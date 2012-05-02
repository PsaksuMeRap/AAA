# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setClass("Positions",contains="list")

setMethod("join",signature(x="Positions",y="Positions"),
		
		function(x,y) {
			return(new("Positions",c(x@.Data,y@.Data)))
		}
)


setMethod("[",signature(x="Positions"),
		function(x,i,j,...,drop=TRUE) {
			positions <- x@.Data[i]
			return(new("Positions",positions))
		}
)

setMethod("[<-",signature(x="Positions"),
		function(x,i,value) {
			x@.Data[i] <- value
			return(x)
		}
)


setMethod("sum","Positions",
		function(x,referenceCurrency) {
			# x: a variable of class Positions (is a list)
			
			if (length(x)==0) {
				if (missing(referenceCurrency)) {
					return(toMoney(amount=0,currency="CHF"))
				} else {
					return(toMoney(amount=0,currency=referenceCurrency))
				}
			}
			
			if (missing(referenceCurrency)) currency <- x[[1]]@security@currency else currency <- referenceCurrency
			balance <- toMoney(amount=0,currency=currency)
			for (i in x) {
				balance <- balance + i@value
			}
			return(balance)
		}
)

setMethod("reweight",signature(x="Positions"),
		function(x,weight) {
			positions <- lapply(x,reweight,weight)
			return(new("Positions",positions))
		}
)

setMethod("fieldsAsCharacter","Positions", 
		function(x,formatWidth=TRUE,referenceCurrency) {
			if (length(x)==0) return(matrix(ncol=0,nrow=0))
		
			if (formatWidth) {
				if (missing(referenceCurrency)) {
					fieldsAsCharacter.matrix <- t(sapply(x,fieldsAsCharacter))
				} else {
					fieldsAsCharacter.matrix <- t(sapply(x,fieldsAsCharacter,referenceCurrency))			
				}
				
				maxFieldsWidth <- apply(nchar(fieldsAsCharacter.matrix),2,max)
				
				for (fieldName in names(maxFieldsWidth)) {

					if (is.element(fieldName,c("amount","referenceCurrencyAmount"))) side <- "left" else side <- "right"
					fieldsAsCharacter.matrix[,fieldName] <- str_pad(fieldsAsCharacter.matrix[,fieldName],width=maxFieldsWidth[[fieldName]],side=side,pad=" ")
				}
				return(fieldsAsCharacter.matrix)
			} else {
				if (missing(referenceCurrency)) {
					return(t(sapply(x,fieldsAsCharacter)))	
				}	else {
					return(t(sapply(x,fieldsAsCharacter,referenceCurrency)))
				}
			}
		}
)

setMethod("as.character","Positions", 
		function(x,formatWidth=TRUE,referenceCurrency) {
			if (length(x)==0) return(vector("character",0))
	
			if (missing(referenceCurrency)) {
				tmp <- fieldsAsCharacter(x,formatWidth=formatWidth)
			} else {
				tmp <- fieldsAsCharacter(x,formatWidth=formatWidth,referenceCurrency)
			}
			result <- sapply(1:nrow(tmp),function(i,tmp){return(paste(tmp[i,],collapse=" / "))},tmp)
			return(result)
		}
)

setMethod("print","Positions",
		function(x,formatWidth=TRUE,referenceCurrency) {

			if (missing(referenceCurrency)) {
				print(as.character(x,formatWidth=formatWidth))
			} else {
				print(as.character(x,formatWidth=formatWidth,referenceCurrency))
			}
		}
)
