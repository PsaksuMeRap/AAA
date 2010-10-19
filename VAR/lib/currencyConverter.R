# TODO: Add comment
# 
# Author: claudio
###############################################################################


convertTo <- function(input,currencyTo) {
	# input: a list <amount,currency> with the amount to be converted
	# currencyTo: the final currency
	
	# create the repository of the exchangeRates if not available
	if (!exists("exchangeRates",envir=repositories,inherits=FALSE)) {
		eval(expression(exchangeRates <- create_repositoryExchangeRates())
				,env=repositories)
	}
	
	rates <- repository$exchangeRates$rates
	
	if (input$currency=="CHF") {
		amountConverted <- input$amount / rates[[currencyTo]]
	} else {
		if (currencyTo=="CHF") {
			amountConverted <- input$amount * rates[[input$currency]]
		} else {
			amountConverted <- input$amount * rates[[input$currency]] /
					rates[[currencyTo]]
		}
	}
	return(list(amount=amountConverted,currency=currencyTo))
}
