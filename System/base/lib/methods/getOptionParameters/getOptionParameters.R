# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("getOptionParameters",def=function(origin,...) standardGeneric("getOptionParameters"))

setMethod("getOptionParameters",signature(origin="PositionOpzioni_su_azioni"),
		function(origin) {
			
			tmp <- as.list(remSpaces(strsplit(origin@security@name,"/")[[1]]))
			fieldNames <- c("quantity","optionType","name","expiryDate","strike","premium","isin","underlyingPrice","contractSize")
			names(tmp) <- fieldNames
			
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp[["strike"]] <- as.numeric(substr(tmp[["strike"]],7,nchar(tmp[["strike"]])))
			tmp[["quantity"]] <- as.numeric(tmp[["quantity"]])
			tmp[["contractSize"]] <- as.numeric(tmp[["contractSize"]])
			tmp[["underlyingPrice"]] <- as.numeric(tmp[["underlyingPrice"]])		
			if (tolower(tmp[["optionType"]])=="put") tmp[["optionType"]] <- "P" else tmp[["optionType"]] <- "C"
			return(tmp)
			
		}
)


setMethod("getOptionParameters",signature(origin="PositionOpzioni_su_divise"),
		function(origin) {
			## example: name = "P/2012-08-17/Strike 1.295/EUR 125000/Premium -8293.75 USD"
			name <- origin@security@name
			tmp1 <- remSpaces(strsplit(name,"/")[[1]])
			names(tmp1) <- c("optionType","expiryDate","strike","amount","premium")
			tmp1 <- as.list(tmp1)
			
			tmp1[["strike"]] <- as.numeric(strsplit(tmp1[["strike"]]," ")[[1]][[2]])
			
			money <- strsplit(tmp1[["amount"]]," ")[[1]]
			tmp1[["amount"]] <- as.numeric(money[[2]])
			tmp1[["underlying"]] <- money[[1]]
			
			premium <- strsplit(tmp1[["premium"]]," ")[[1]]
			tmp1[["premium"]] <-  as.numeric(premium[[2]])
			tmp1[["numeraire"]] <-  premium[[3]]
		
			return(tmp1)
		}

)

