# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_accruedInterest <- function(money,date) {
	accruedInterest <- list(money=money,date=date)
	class(accruedInterest) <- "accruedInterest"
	return(accruedInterest)
}
