# TODO: Add comment
# 
# Author: Claudio
###############################################################################


tradesToPositionsFactory <- function(fileName,directory) {
	# create the blRequestHandler required from tradeToSecurityFactory
	# (a bloomberg repository must exists!)
	blRequestHandler <- create_BloombergRequestHandler()
	
	# create the message information with trades
	message <- messageFactory(fileName,directory)

	# create the corresponding securities and fill the blRequestHandler
	if (length(message@trades)>0) {
		securities <- lapply(message@trades,tradeToSecurityFactory,blRequestHandler)
	} else {
		return(new("Positions"))
	}
	
	# update the bloombergData repository
	blData <- repositories$bloombergData
	blData <- blRequestHandler[["execute"]](blData)
	
	if (!identical(repositories$bloombergData,blData)) {
		# update the local repository
		assign("bloombergData",blData,pos=repositories)
		# save it to data/bloomberg directory
		directory <- file.path(systemOptions[["homeDir"]],"data","bloomberg",message[["portfolioName"]])
		saveLastObject(blData,"bloombergData.RData",directory)
	}
	
	positions <- new("Positions")
	positionsIndex <- 0

	for (index in 1:length(securities)) {
		positionTmp <- tradeToPositionFactory(securities[[index]],message@trades[[index]],blData)
		positionsTmp <- tradeToPositionsFactory(positionTmp,trade[[index]])
		nbNewPositions <- length(positionsTmp)
		positions[(positionsIndex+1):(positionsIndex+nbNewPositions)] <- positionsTmp
		positionsIndex <- positionsIndex + nbNewPositions
	}
	return (positions)
}
