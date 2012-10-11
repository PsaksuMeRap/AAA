# TODO: Add comment
# 
# Author: claudio
###############################################################################




create_futureEquityIndicesDB <- function() {
	# future on the Swiss Market Index
	swissMarketIndex <- list(indexName="swissMarketIndex",bloombergCode="SM",bloombergName="SWISS MKT IX FUTR",name="SMI futures")
	
	# future on the DAX index
	dax <- list(indexName="dax",bloombergCode="GX",bloombergName="DAX INDEX FUTURE",name="Dax futures")
	
	# future on the S&P500 index
	standardAndPoors <- list(indexName="standardAndPoors",bloombergCode="SP",bloombergName="S&P 500 FUTURE",name="S&P500 Futures")
	
	# future on the CBOE VIX index
	vix <- list(indexName="vix",bloombergCode="UX",bloombergName="CBOE VIX FUTURE",name="CBOE Volatility Index (VIX) Futures")
	
	# future on the Eurostoxx 50 index
	eurostoxx50 <- list(indexName="eurostoxx50",bloombergCode="VG",bloombergName="EURO STOXX 50",name="Eurostoxx50 Futures")
	
	futureEquityIndicesDB <- data.frame(swissMarketIndex,stringsAsFactors=FALSE)
	futureEquityIndicesDB <- rbind(futureEquityIndicesDB,dax,standardAndPoors,
			vix,eurostoxx50)
	
	return(futureEquityIndicesDB)
}

