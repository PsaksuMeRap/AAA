# TODO: Add comment
# 
# Author: claudio
###############################################################################

# crea la classe Money
setClass("Money",
		representation(amount="Amount",currency="Currency"),
		prototype(amount=new("Amount",0),currency=new("Currency","CHF"))
)

setMethod("*",signature("numeric","Money"),
		function(e1,e2) {
			# e1: a number
			e2@amount <- e1*e2@amount
			return(e2)
		}
)

setMethod("*",signature("Money","numeric"),
		function(e1,e2) {
			# e1: a Money
			e1@amount <- e2*e1@amount
			return(e1)
		}
)

setMethod("+",signature("Money","Money"),
		function(e1,e2) {
			# e1: a money
			if(e1@currency != e2@currency) e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			e1@amount <- e1@amount + e2@amount
			return(e1)
		}
)


setMethod("/",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			# e1: a money
			# e2: a meney, the divisor, i.e. the money used to divide e1
			
			if (e1@currency!=e2@currency) e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			return(unclass(e1@amount / e2@amount))
		}
)


setMethod("as.character","Money",
		function(x) {
			# x: a money
			getAmountWidth <- function(amount) {
				# questa funzione calcola il numero di caratteri da usare per
				# la formattazione dell'output dell'importo
				nbChar <- nchar(as.character(floor(abs(amount))))
				width <- nbChar %/% 3
				width <- nbChar + width + 3
				if (amount < 0) width <- width + 1 
				
				return(width)
			}
			
			width <- getAmountWidth(x@amount)
			number <- formatC(x@amount,width=width,big.mark = "'",
					decimal.mark = ".",format="f",digits=2)
			string = paste(x@currency,number)
			return(string)
		}
)


setMethod("print","Money",
		function(x) {
			print(as.character(x))
		}
)



setGeneric("toMoney",def=function(amount,currency) standardGeneric("toMoney"))

setMethod("toMoney",signature(amount="Amount",currency="Currency"),
		function(amount=new("Amount",0),currency=new("Currency","CHF")) {
			return(new("Money",amount=amount,currency=currency))
		}
)

setMethod("toMoney",signature(amount="numeric",currency="Currency"),
		function(amount=0,currency=new("Currency","CHF")) {
			return(new("Money",amount=new("Amount",amount),currency=currency))
		}
)

setMethod("toMoney",signature(amount="Amount",currency="character"),
		function(amount=new("Amount",0),currency="CHF") {
			return(new("Money",amount=amount,currency=new("Currency",currency)))
		}
)

setMethod("toMoney",signature(amount="numeric",currency="character"),
		function(amount=0,currency="CHF") {
			return(new("Money",amount=new("Amount",amount),currency=new("Currency",currency)))
		}
)

setMethod(">",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			if (!identical(e1@currency,e2@currency)) {
				# exchange the money in the desired currency
				e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			}
			# excecute the check
			return(e1@amount > e2@amount)	
		}
)

setMethod(">=",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			if (!identical(e1@currency,e2@currency)) {
				# exchange the money in the desired currency
				e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			}
			# excecute the check
			return(e1@amount >= e2@amount)	
		}
)

setMethod("<",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			if (!identical(e1@currency,e2@currency)) {
				# exchange the money in the desired currency
				e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			}
			# excecute the check
			return(e1@amount < e2@amount)	
		}
)

setMethod("<=",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			if (!identical(e1@currency,e2@currency)) {
				# exchange the money in the desired currency
				e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			}
			# excecute the check
			return(e1@amount <= e2@amount)	
		}
)

setMethod("==",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			if (!identical(e1@currency,e2@currency)) {
				# exchange the money in the desired currency
				e2 <- repositories$exchangeRates$exchange(e2,e1@currency)
			}
			# excecute the check
			return(abs(e1@amount- e2@amount) < systemOptions[["eq.tolerance"]]) 	
		}
)

setMethod("!=",signature(e1="Money",e2="Money"),
		function(e1,e2) {
			return(!(e1 == e2))
		}
)