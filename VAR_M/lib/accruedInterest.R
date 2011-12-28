# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_accruedInterest <- function(money,paymentDate) {
	accruedInterest <- list(money=money,paymentDate=paymentDate)
	class(accruedInterest) <- "accruedInterest"
	return(accruedInterest)
}
