# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("getOptionParameters",def=function(origin,...) standardGeneric("getOptionParameters"))


setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_divise"),
		function(origin) {
			
			## example: name = "Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
			## the first string contains "Call 17-08-12 Strike 1.295 EUR -250000" and the second "(1930 USD)"				
			tmp1 <- as.list(stringr::str_trim(strsplit(origin@Nome,"Premio")[[1]]))
			
			## extract from the first component of tmp1
			tmp2 <- as.list(stringr::str_trim(strsplit(tmp1[[1]]," ")[[1]]))
			fieldNames <- c("optionType","expiryDate","strikeLabel","strike","underlying","quantity")
			names(tmp2) <- fieldNames
			tmp2[["strikeLabel"]] <- NULL
			tmp2[["expiryDate"]] <- format(strptime(tmp2[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp2[["strike"]] <- as.numeric(tmp2[["strike"]])
			tmp2[["quantity"]] <- as.numeric(tmp2[["quantity"]])
			if (tolower(tmp2[["optionType"]])=="put") tmp2[["optionType"]] <- "Put" else tmp2[["optionType"]] <- "Call"
			
			return(tmp2)
		}

)


setMethod("getOptionParameters",signature(origin="Ayrton_Opzioni_su_azioni"),
		function(origin) {
			tmp <- as.list(stringr::str_trim(strsplit(origin@Nome,"/")[[1]]))
			fieldNames <- c("quantity","optionType","name","expiryDate","strike","Premio","isin")
			names(tmp) <- fieldNames
			
			tmp[["expiryDate"]] <- format(strptime(tmp[["expiryDate"]],format="%d-%m-%y"),"%Y-%m-%d")
			tmp[["strike"]] <- as.numeric(substr(tmp[["strike"]],7,nchar(tmp[["strike"]])))
			tmp[["quantity"]] <- as.numeric(tmp[["quantity"]])
			if (tolower(tmp[["optionType"]])=="put") tmp[["optionType"]] <- "Put" else tmp[["optionType"]] <- "Call"
			return(tmp)
			
			}

)