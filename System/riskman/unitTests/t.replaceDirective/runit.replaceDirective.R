# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldReplacePositionFutures_EQ <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$Futures_EQ1,repository$equity2,repository$bond1)
	
	result <- replaceDirective(positions[[2]])
	
	checkEquals(length(result),2)
	checkEquals(is(result[[1]],"PositionFutures_EQ"),TRUE)
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@value,-1 * result[[2]]@value)
	
}


test.shouldReplacePositionOpzioni_su_divise <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$Opzioni_su_divise1,repository$Opzioni_su_divise2,
			repository$Opzioni_su_divise3,repository$Opzioni_su_divise4)
	
	# for "PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"	
	result <- replaceDirective(positions[[1]])	
	checkEquals(length(result),2)
	
	checkEquals(is(result[[1]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","EUR"))
	checkEquals(result[[1]]@value,toMoney(-125000,"EUR"))	
	
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[2]]@security@currency,new("Currency","USD"))
	checkEquals(result[[2]]@value,toMoney(125000*1.295,"USD"))
	
	
	# for "Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
	result <- replaceDirective(positions[[2]])
	checkEquals(length(result),2)
	
	checkEquals(is(result[[1]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","EUR"))
	checkEquals(result[[1]]@value,toMoney(-250000,"EUR"))	
	
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[2]]@security@currency,new("Currency","USD"))
	checkEquals(result[[2]]@value,toMoney(250000*1.295,"USD"))
	
	# for "Call 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"
	result <- replaceDirective(positions[[3]])	
	checkEquals(length(result),2)
	
	checkEquals(is(result[[1]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","EUR"))
	checkEquals(result[[1]]@value,toMoney(125000,"EUR"))	
	
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[2]]@security@currency,new("Currency","USD"))
	checkEquals(result[[2]]@value,toMoney(-125000*1.295,"USD"))
	
	
	# for "PUT 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
	result <- replaceDirective(positions[[4]])
	checkEquals(length(result),2)
	
	checkEquals(is(result[[1]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","EUR"))
	checkEquals(result[[1]]@value,toMoney(250000,"EUR"))	
	
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[2]]@security@currency,new("Currency","USD"))
	checkEquals(result[[2]]@value,toMoney(-250000*1.295,"USD"))

}


test.shouldReplacePositionOpzioni_su_azioni <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- new("Positions",list(repository$Opzioni_su_azioni1,repository$Opzioni_su_azioni2,
			repository$Opzioni_su_azioni3,repository$Opzioni_su_azioni4))
	
	## short call
	# for "-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
	result <- replaceDirective(positions[[1]])	
	checkEquals(length(result),1)
	checkEquals(class(result[[1]])[[1]],"PositionEquity")
	checkEquals(result[[1]]@quantity,-100*10)
	checkEquals(result[[1]]@security@currency,new("Currency","CHF"))
	checkEquals(result[[1]]@value,toMoney(-100*10*337.90,"CHF"))
	
	## short put
	# for "-500 / PUT / Credit Suisse Group Na / 21-12-12 / Strike 46 / Premio(112267 CHF) / CH0012138530 / 17.71 / 10"
	result <- replaceDirective(positions[[2]])
	checkEquals(length(result),2)
	
	checkEquals(is(result[[1]],"PositionEquity"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","CHF"))
	checkEquals(result[[1]]@value,toMoney(5000*17.71,"CHF"))	
	
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[2]]@security@currency,new("Currency","CHF"))
	checkEquals(result[[2]]@value,toMoney(-5000*46,"CHF"))
	
	## long put
	# for '100 / PUT / Syngenta AG / 17-02-12 / Strike 290 / Premio(-5500 CHF) / CH0011027469 / 337.90'
	result <- replaceDirective(positions[[3]])	
	checkEquals(length(result),1)	
	
	checkEquals(is(result[[1]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","CHF"))
	checkEquals(result[[1]]@value,toMoney(0,"CHF"))
	
	## long call
	# for '500 / Call / Credit Suisse Group Na / 21-12-12 / Strike 46 / Premio(-112267 CHF) / CH0012138530 / 17.71'
	result <- replaceDirective(positions[[4]])
	checkEquals(length(result),2)
	
	checkEquals(is(result[[1]],"PositionEquity"),TRUE)
	checkEquals(result[[1]]@security@currency,new("Currency","CHF"))
	checkEquals(result[[1]]@value,toMoney(500*10*17.71,"CHF"))	
	
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[2]]@security@currency,new("Currency","CHF"))
	checkEquals(result[[2]]@value,toMoney(-500*10*46,"CHF"))
	
}
