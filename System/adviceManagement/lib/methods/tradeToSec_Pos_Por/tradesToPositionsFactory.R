# TODO: Add comment
# 
# Author: Claudio
###############################################################################


tradesToPositionsFactory <- function(fileName,directory) {
	# create the blRequestHandler required from tradeToSecurityFactory
	# (a bloomberg repository must exists!)
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	positions <- new("Positions")
	nbPositions <- 0
	for (trade in trades) {	
		security <- tradeToSecurityFactory(trade,blRequestHandler)
		positionTmp <- tradeToPositionFactory(security,trade,blData)
		positionsTmp <- tradeToPositionsFactory(positionTmp,trade)
		nbNewPositions <- length(positionsTmp)
		positions[(nbPositions+1):(nbPositions+nbNewPositions)] <- positionsTmp
		nbPositions <- nbPositions + nbNewPositions
	}
	return (positions)
}
