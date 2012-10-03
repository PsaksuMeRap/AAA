# TODO: Add comment
# 
# Author: claudio
###############################################################################


parseOptionOnEquityName <- function(name) {
	tmp <- strsplit(name,"/")[[1]]
	tmp <- str_trim(tmp)
	names(tmp) <- c("quantity","callPut","underlyingName","maturity","strike","premium","ISIN","underlyingPrice","contractSize")
	tmp <- as.list(tmp)
	tmp[["quantity"]] <- as.numeric(tmp[["quantity"]])
	tmp[["contractSize"]] <- as.numeric(tmp[["contractSize"]])
	
	if (tmp[["callPut"]]=="Call") tmp[["callPut"]] <- "C" else tmp[["callPut"]] <- "P"
	tmp[["maturity"]] <- format(strptime(tmp[["maturity"]],format="%d-%m-%y"),"%d-%m-%Y")
	tmp[["strike"]] <- substr(tmp[["strike"]],8,nchar(tmp[["strike"]]))
	tmp[["underlyingPrice"]] <- as.numeric(tmp[["underlyingPrice"]])
	#"-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
	return(tmp)
}
