# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldImportPricePositionMultiplicatorFactors <- function() {
	allowedCurrencies <- c(
			"AUD",
			"BRL",
			"CAD",
			"CNY",
			"DKK",
			"EUR",
			"GBP",
			"IDR",
			"INR",
			"JPY",
			"MXN",
			"NOK",
			"NZD",
			"PLN",
			"RUB",
			"SEK",
			"SGD",
			"TRY",
			"USD",
			"ZAR"
	)


	securities <- apply(expand.grid(allowedCurrencies,allowedCurrencies),1,function(x) paste(x,collapse=""))
	y <- paste(allowedCurrencies,allowedCurrencies,sep="")
	toRemove <- is.element(securities,y)
	securities <- securities[!toRemove]
	securities <- paste(securities," CURNCY")
	securities <- paste(allowedCurrencies,"CHF Curncy",sep="")
	conn <- blpConnect()
	result <- bdp(conn, securities=securities, fields="PX_POS_MULT_FACTOR")
	blpDisconnect(conn)
	
	PX_POS_MULT_FACTOR
	"	AUDCHF Curncy              1e+00"
	"	BRLCHF Curncy              1e+00"
	"	CADCHF Curncy              1e+00"
	"	CNYCHF Curncy              1e+00"
	"	DKKCHF Curncy              1e-02"
	"	EURCHF Curncy              1e+00"
	"	GBPCHF Curncy              1e+00"
	"	IDRCHF Curncy              1e-04"
	"	INRCHF Curncy              1e+00"
	"	JPYCHF Curncy              1e-02"
	"	MXNCHF Curncy              1e+00"
	"	NOKCHF Curncy              1e-02"
	"	NZDCHF Curncy              1e+00"
	"	PLNCHF Curncy              1e+00"
	"	RUBCHF Curncy              1e+00"
	"	SEKCHF Curncy              1e-02"
	"	SGDCHF Curncy              1e+00"
	"	TRYCHF Curncy              1e+00"
	"	USDCHF Curncy              1e+00"
	"	ZARCHF Curncy              1e+00"
	
	
	
}
