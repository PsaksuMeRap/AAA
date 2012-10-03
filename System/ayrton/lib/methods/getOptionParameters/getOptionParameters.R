# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("getOptionParameters",def=function(origin,...) standardGeneric("getOptionParameters"))

setMethod("getOptionParameters",signature(origin="AyrtonPosition"),
		function(origin) {
			if (origin@ID_strumento==18) class(origin) <- "Ayrton_Opzioni_su_azioni"
			if (origin@ID_strumento==19) class(origin) <- "Ayrton_Opzioni_su_divise"
			if (origin@ID_strumento==20) class(origin) <- "Ayrton_Opzioni_su_obbligazioni"
			return(getOptionParameters(origin))
		}
)

setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_divise"),
		function(origin) {
			
			## example: name = "Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
			## the first string contains "Call 17-08-12 Strike 1.295 EUR -250000" and the second "(1930 USD)"				
			tmp1 <- as.list(stringr::str_trim(strsplit(origin@Nome,"Premio\\(")[[1]]))
			premium <- tmp1[[2]]
			
			## extract from the first component of tmp1
			tmp2 <- as.list(stringr::str_trim(strsplit(tmp1[[1]]," ")[[1]]))
			fieldNames <- c("optionType","expiryDate","strikeLabel","strike","underlying","quantity")
			names(tmp2) <- fieldNames
			tmp2[["strikeLabel"]] <- NULL
			tmp2[["expiryDate"]] <- format(strptime(tmp2[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp2[["strike"]] <- as.numeric(tmp2[["strike"]])
			tmp2[["quantity"]] <- as.numeric(tmp2[["quantity"]])
			if (tolower(tmp2[["optionType"]])=="put") tmp2[["optionType"]] <- "P" else tmp2[["optionType"]] <- "C"
			premiumNchar <- nchar(premium)
			tmp2[["premium"]] <- as.numeric(substr(premium,1,premiumNchar-4))
			tmp2[["numeraire"]] <- substr(premium,premiumNchar-3,premiumNchar-1)
			return(tmp2)
		}

)

setMethod("getOptionParameters",signature(origin="PositionOpzioni_su_divise"),
		function(origin) {
			
			## example: name = "Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
			## the first string contains "Call 17-08-12 Strike 1.295 EUR -250000" and the second "(1930 USD)"				
			name <- origin@security@name
			tmp1 <- as.list(stringr::str_trim(strsplit(name,"Premio\\(")[[1]]))
			premium <- tmp1[[2]]
			
			## extract from the first component of tmp1
			tmp2 <- as.list(stringr::str_trim(strsplit(tmp1[[1]]," ")[[1]]))
			fieldNames <- c("optionType","expiryDate","strikeLabel","strike","underlying","quantity")
			names(tmp2) <- fieldNames
			tmp2[["strikeLabel"]] <- NULL
			tmp2[["expiryDate"]] <- format(strptime(tmp2[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp2[["strike"]] <- as.numeric(tmp2[["strike"]])
			tmp2[["quantity"]] <- as.numeric(tmp2[["quantity"]])
			if (tolower(tmp2[["optionType"]])=="put") tmp2[["optionType"]] <- "Put" else tmp2[["optionType"]] <- "Call"
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
			tmp <- str_trim(tmp)
			names(tmp) <- c("optionType","expiryDate","strike_label","strike","currency","amount","premium","premiumCurrency","isin")
			tmp <- as.list(tmp)
			
			if (tolower(tmp[["optionType"]])=="put") tmp[["optionType"]] <- "P" else tmp[["optionType"]] <- "C"
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			#"PUT 17-08-12 Strike 103.5 EUR 125000 Premio(-345.45 EUR) EU0011027469"
			return(tmp)
		}
)

setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_azioni"),
		function(origin) {

			tmp <- as.list(stringr::str_trim(strsplit(origin@Nome,"/")[[1]]))
			fieldNames <- c("quantity","optionType","name","expiryDate","strike","Premio","isin","underlyingPrice")
			names(tmp) <- fieldNames
			
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp[["strike"]] <- as.numeric(substr(tmp[["strike"]],7,nchar(tmp[["strike"]])))
			tmp[["quantity"]] <- as.numeric(tmp[["quantity"]])
			tmp[["underlyingPrice"]] <- as.numeric(tmp[["underlyingPrice"]])		
			if (tolower(tmp[["optionType"]])=="put") tmp[["optionType"]] <- "P" else tmp[["optionType"]] <- "C"
			return(tmp)
			
			}

)

setMethod("getOptionParameters",signature(origin="PositionOpzioni_su_azioni"),
		function(origin) {

			tmp <- as.list(stringr::str_trim(strsplit(origin@security@name,"/")[[1]]))
			fieldNames <- c("quantity","optionType","name","expiryDate","strike","Premio","isin","underlyingPrice")
			names(tmp) <- fieldNames
			
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp[["strike"]] <- as.numeric(substr(tmp[["strike"]],7,nchar(tmp[["strike"]])))
			tmp[["quantity"]] <- as.numeric(tmp[["quantity"]])
			tmp[["underlyingPrice"]] <- as.numeric(tmp[["underlyingPrice"]])		
			if (tolower(tmp[["optionType"]])=="put") tmp[["optionType"]] <- "Put" else tmp[["optionType"]] <- "Call"
			return(tmp)
		}
)