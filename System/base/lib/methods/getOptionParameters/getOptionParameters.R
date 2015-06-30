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

setMethod("getOptionParameters",signature(origin="PositionOpzioni_su_obbligazioni"),
		function(origin) {
			
			name <- origin@security@name
			tmp <- strsplit(name," ")[[1]]
			tmp <- remSpaces(tmp)
			names(tmp) <- c("optionType","expiryDate","strike_label","strike","underlyingCurrency","amount","premium","numeraire","isin")
			tmp <- as.list(tmp)
			tmp[["strike"]] <- as.numeric(tmp[["strike"]])
			tmp[["amount"]] <- as.numeric(tmp[["amount"]])
			tmp[["premium"]] <- as.numeric(substr(tmp[["premium"]],8,nchar(tmp[["premium"]])))
			tmp[["numeraire"]] <- substr(tmp[["numeraire"]],1,3)
			if (tolower(tmp[["optionType"]])=="put") tmp[["optionType"]] <- "P" else tmp[["optionType"]] <- "C"
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			#"PUT 17-08-12 Strike 103.5 EUR 125000 Premio(-345.45 EUR) EU0011027469"
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

setMethod("getOptionParameters",signature(origin="PositionStrutturati_FX"),
		function(origin) {
			## esempio con origin@security@name: "20150918 - Put Warrant Vontobel EUR/CHF CHF 1"
			
			
			name <- origin@security@name
			tmp <- strsplit(name," ")[[1]]
			## rimuovi "-"
			tmp <- tmp[-2]
			tmp <- remSpaces(tmp)
			names(tmp) <- c("expiryDate","optionType","Nome","underlying","strikeIn-XXX","strike")
			tmp[["underlying"]] <- substr(tmp[["underlying"]],1,3)
			
			tmp <- as.list(tmp)
			tmp[["strike"]] <- new("Money",
					amount=new("Amount",as.numeric(tmp[["strike"]])),
					currency=new("Currency",tmp[["strikeIn-XXX"]]))
			
			tmp[["legCurrency2"]] <- origin@quantity
			
			tmp[["legCurrency1"]] <- origin@otherLag
			
			tmp[["premium"]] <- origin@
			
			
			if (tolower(tmp[["optionType"]])=="put") {
				tmp[["optionType"]] <- "P" 
			} else {
				tmp[["optionType"]] <- "C"
			}
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%Y%m%d"),"%Y-%m-%d")
			

			return(tmp)

			
		}
)