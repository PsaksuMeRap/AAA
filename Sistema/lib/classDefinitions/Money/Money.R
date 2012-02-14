# TODO: Add comment
# 
# Author: claudio
###############################################################################

# crea la classe Money
setClass("Money",
		representation(amount="Amount",currency="Currency"),
		prototype(amount=new("Amount",0),currency=new("Currency","CHF"))
)


fun <- function(x,moneyToAdd) {
	# x: a money
	if(x@currency != moneyToAdd@currency) moneyToAdd <- repositories$exchangeRates$exchange(moneyToAdd,x@currency)
	x@amount <- x@amount + moneyToAdd@amount
	return(x)
}
setMethod("sum","Money",fun)


fun <- function(x,divisor) {
	# x: a money
	# divisor: the money used to divide x
	
	if (x@currency!=divisor@currency) divisor <- repositories$exchangeRates$exchange(divisor,x@currency)
	return(x@amount / divisor@amount)
}
setMethod("divide","Money",fun)



fun <- function(x) {
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
setMethod("toString","Money",fun)


fun <- function(x) {
	print(toString(x))
}
setMethod("print","Money",fun)



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
