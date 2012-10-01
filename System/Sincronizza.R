
workingDir <- getwd()

homeDir <- "C:/riskman"

library(tcltk)


directoryNames <- c("asymmetricEquity",
		"fixedIncome","globalEconomy",
		"globalEquity","multistrategy")

isOk <- TRUE
for (directoryName in directoryNames) {
	from <- file.path(workingDir,directoryName,"portfolio.RData")
	to <- file.path(homeDir,"data","portfolios",directoryName,"portfolio.RData")
 
	if (!file.copy(from,to,overwrite=TRUE)) {
		messageText <- paste("Warning, the portfolio of '",directoryName,
				"' has not been copied correctly!",sep="")
		tkmessageBox(message=messageText,icon="warning")
	isOk <- FALSE
	}

}	

if (isOk) tkmessageBox(message="The copy procedure has terminated correctly.",icon="warning")

