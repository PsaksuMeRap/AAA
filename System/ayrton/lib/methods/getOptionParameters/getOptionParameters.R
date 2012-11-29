# TODO: Add comment
# 
# Author: claudio
###############################################################################


setMethod("getOptionParameters",signature(origin="AyrtonPosition"),
		function(origin) {
			if (origin@ID_strumento==18) {
				class(origin) <- "Ayrton_Opzioni_su_azioni"
				return(getOptionParameters(origin))
			}
			
			if (origin@ID_strumento==19) {
				class(origin) <- "Ayrton_Opzioni_su_divise"
				return(getOptionParameters(origin))
			}
			
			if (origin@ID_strumento==20) {
				class(origin) <- "Ayrton_Opzioni_su_obbligazioni"
				return(getOptionParameters(origin))
			}
			
			stop(paste("getOptionParameters not defined for an AyrtonPosition with ID_strumento",origin@ID_strumento))
		}
)


setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_azioni"),
		function(origin) {
			
			tmp <- as.list(remSpaces(strsplit(origin@Nome,"/")[[1]]))
			fieldNames <- c("quantity","optionType","name","expiryDate","strike","premium","isin","underlyingPrice","contractSize")
			
			# check the the number of fields is complete
			if (length(tmp)<9) {
				messageString <- paste("Fields problems in the option position:",origin@Nome,
						"\n\nSome field is missing?")
				ok <- tkmessageBox(message=messageString,icon="warning")
				stop(paste("Procedure terminated from getOptionParameters:",origin@Nome))
			}
			
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


setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_divise"),
		function(origin) {
			
			## example: name = "Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
			## the first string contains "Call 17-08-12 Strike 1.295 EUR -250000" and the second "1930 USD)"				
			tmp1 <- as.list(remSpaces(strsplit(origin@Nome,"Premio\\(")[[1]]))
			premium <- tmp1[[2]]
			
			## extract from the first component of tmp1
			tmp2 <- as.list(remSpaces(strsplit(tmp1[[1]]," ")[[1]]))
			fieldNames <- c("optionType","expiryDate","strikeLabel","strike","underlying","amount")
			names(tmp2) <- fieldNames
			tmp2[["strikeLabel"]] <- NULL
			tmp2[["expiryDate"]] <- format(strptime(tmp2[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp2[["strike"]] <- as.numeric(tmp2[["strike"]])
			tmp2[["amount"]] <- as.numeric(tmp2[["amount"]])
			if (tolower(tmp2[["optionType"]])=="put") tmp2[["optionType"]] <- "P" else tmp2[["optionType"]] <- "C"
			premiumNchar <- nchar(premium)
			tmp2[["premium"]] <- as.numeric(substr(premium,1,premiumNchar-4))
			tmp2[["numeraire"]] <- substr(premium,premiumNchar-3,premiumNchar-1)
			return(tmp2)
		}
)


setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_obbligazioni"),
		function(origin) {
			name <- origin@Nome
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
