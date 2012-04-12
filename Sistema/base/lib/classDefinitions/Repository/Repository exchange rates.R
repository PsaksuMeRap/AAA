# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass(
		"Repository exchange rates",
		representation(exchange="function",rates="numeric"),
		prototype(exchange=function(money,toCurrency) {
					# money: an Object of class Money containing the amount to be converted
					# toCurrency: the final currency (of Class "Currency")
					
					amount <- money@amount
					fromCurrency <- money@currency
					
					if (identical(toCurrency,new("Currency","CHF")) | identical(toCurrency,new("Currency","CHr"))) {
						result <- new("Money",amount=amount*repository$rates[[fromCurrency]],currency=toCurrency)
						return(result)
					}
					
					areAvailable <- is.element(c(fromCurrency,toCurrency),names(repository$rates))
					if (any(!areAvailable)) {
						print(paste("Error",c(fromCurrency,toCurrency)[!areAvailable],"not available."))
					}
					result <- new("Money",amount=amount * repository$rates[[fromCurrency]] / repository$rates[[toCurrency]],
							currency=toCurrency)
					return(result)
				}
		),
		contains="Repository"
)



setClass("test",representation(f="environment",x="numeric"))
