# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_BloombergRequestHandler <- function() {
	blRequestHandler <- new.env()
	class(blRequestHandler) <- "BloombergRequestHandler"
	
	blRequestHandler[["requests"]] <- list()
	
	blRequestHandler[["collect"]] <- function(blId,fieldId) {
		blRequestHandler$requests[[paste(blId,fieldId,sep="__")]] <<- list(blId=blId,fieldId=fieldId,requestDateTime=Sys.time())
	}
	
	blRequestHandler[["isToRemove"]] <- function(request,blData) {
		identifier <- paste(request$blId,request$fieldId,sep="__")
		match <- blData[[identifier]]
		
		# if the field is not in blData the request must be executed
		if (is.null(match)) return(FALSE)
		
		dateDiff <- Sys.time() - match@dateLastUpdate
		if (dateDiff - systemOptions[["bloombergUpdateInterval"]]>=0) {
			return(FALSE) 
		} else {
			return(TRUE)
		}
	}
	
	blRequestHandler[["execute"]] <- function(blData) {

		if (missing(blData)) {
			blData <- new("BloombergData")		
			if (length(blRequestHandler[["requests"]])==0) return(blData)

		} else {
			# remove the data entries having a value not older than systemOptions[["bloombergUpdateInterval"]] seconds
			if (length(blRequestHandler[["requests"]])==0) return(blData)
			
			isToRemove <- sapply(blRequestHandler[["requests"]],blRequestHandler[["isToRemove"]],blData,USE.NAMES=FALSE)
			
			if (any(isToRemove)) blRequestHandler[["requests"]] <<- blRequestHandler[["requests"]][!isToRemove]
			
		}
		# extract all securities (repeated if necessary)
		blId <- extractFromList(blRequestHandler[["requests"]],"blId")
		# extract all fields Id
		fieldId <- extractFromList(blRequestHandler[["requests"]],"fieldId")
		
		# excecute request by blId		
		conn <- blpConnect()
		for (security in unique(blId)) {
			ok <- blId == security
			result <- bdp(conn, securities=security, fields=fieldId[ok])
			for (field in fieldId[ok]) {
				blData <- add(new("BloombergDataEntry",blId=security,fieldId=field,
								value=result[security,field],dateLastUpdate=Sys.time()),blData)
			}
		}
		blpDisconnect(conn)
		

		# empty the request list
		blRequestHandler[["requests"]] <<- list()
		return(blData)		
	}
	
	return(blRequestHandler)
}
