# TODO: Add comment
# 
# Author: ortellic
###############################################################################



test.aligneFutureContractsAtDate <- function() {
	
	data1 <- data.frame(open=c(NA,NA,NA),low=c(NA,NA,NA),settlement=c(NA,NA,NA))
	rownames(data1) <- c("2004-06-14","2004-06-15","2004-06-16")
	contract1 <- create_contract(name="test3",
			settlementDate="2004-03-14",
			lastTradeDate="2004-03-15",
			data=data1)
	
	data2 <- data.frame(open=c(1,2,3),low=c(1,2,3),settlement=c(1,2,3))
	rownames(data2) <- c("2004-06-14","2004-06-15","2004-06-16")
	contract2 <- create_contract(name="test2",
			settlementDate="2004-06-21",
			lastTradeDate="2004-06-20",
			data=data2)
	
	data3 <- data.frame(open=c(4,5,6),low=c(4,5,6),settlement=c(4,5,6))
	rownames(data3) <- c("2004-06-14","2004-06-15","2004-06-16")
	contract3 <- create_contract(name="test1",
			settlementDate="2004-07-17",
			lastTradeDate="2004-06-18",
			data=data3)
	
	contracts <- list(contract1,contract2,contract3)

	result <- aligneFutureContractsAtDate("2004-06-14",contracts)
	checkEquals(result,list(Date="2004-06-14",Price1=1,Switch=FALSE))
	
	result <- aligneFutureContractsAtDate("2004-06-14",contracts,nbDesiredFutures=2)
	checkEquals(result,list(Date="2004-06-14",Price1=1,Price2=4,Switch=FALSE))
	
	result <- aligneFutureContractsAtDate("2004-06-14",contracts,nbDesiredFutures=3)
	checkEquals(result,list(Date="2004-06-14",Price1=1,Price2=4,Price3=NA_real_,Switch=FALSE))
	
	result <- aligneFutureContractsAtDate("2006-06-14",contracts,nbDesiredFutures=3)
	checkEquals(result,list(Date="2006-06-14",Price1=NA_real_,Price2=NA_real_,Price3=NA_real_,Switch=FALSE))
	
	result <- aligneFutureContractsAtDate("2004-06-14",contracts,nbBusDaysBefSettlement=7)
	checkEquals(result,list(Date="2004-06-14",Price1=4,Switch=FALSE))
	
	result <- aligneFutureContractsAtDate("2004-06-14",contracts,nbBusDaysBefSettlement=5)
	checkEquals(result,list(Date="2004-06-14",Price1=1,Switch=TRUE))
	
	result <- aligneFutureContractsAtDate("2004-06-15",contracts,nbBusDaysBefSettlement=5)
	checkEquals(result,list(Date="2004-06-15",Price1=5,Switch=FALSE))
	
}