
workingDir <- getwd()

homeDir <- "C:/riskman"

library(tcltk)

# copy the portfolios
directoryNames <- c("asymmetricEquity",
		"fixedIncome","globalEconomy",
		"globalEquity","multistrategy")

isOk <- TRUE
for (directoryName in directoryNames) {
	from <- file.path(workingDir,"dataNew","portfolios",directoryName,"portfolio.RData")
	to <- file.path(homeDir,"data","portfolios",directoryName,"portfolio.RData")
 
	if (!file.copy(from,to,overwrite=TRUE)) {
		messageText <- paste("Warning, the portfolio of '",directoryName,
				"' has not been copied correctly!",sep="")
		tkmessageBox(message=messageText,icon="warning")
	isOk <- FALSE
	}

}	

if (!file.copy(from,to,overwrite=TRUE)) {
	messageText <- paste("Warning, the portfolio of '",directoryName,
			"' has not been copied correctly!",sep="")
	tkmessageBox(message=messageText,icon="warning")
	isOk <- FALSE
}

# copy the DBEquities
from <- file.path(workingDir,"dataNew","DBEquities","DBEquities.RData")
to <- file.path(homeDir,"data","DBEquities","DBEquities.RData")

if (!file.copy(from,to,overwrite=TRUE)) {
	messageText <- paste("Warning, the DBEquities.RData file has not been copied correctly!",sep="")
	tkmessageBox(message=messageText,icon="warning")
	isOk <- FALSE
}

if (isOk) tkmessageBox(message="The copy procedure has terminated correctly.",icon="warning")

