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
	
	blRequestHandler[["execute"]] <- function() {
		if (length(blRequestHandler[["requests"]])==0) return(list())
		
		# extract all securities (repeated if necessary)
		blId <- extractFromList(blRequestHandler[["requests"]],"blId")
		# extract all fields Id
		fieldId <- extractFromList(blRequestHandler[["requests"]],"fieldId")
		
		# excecute request by blId
		blData <- new("BlommbergData")
		conn <- blpConnect()
		for (security in unique(blId)) {
			ok <- blId == security
			result <- bdp(conn, securities=security, fields=fieldId[ok])
			for (field in fieldId[ok]) {
				blData <- add(new("BloombergDataEntry",blId=security,fieldId=field,
								value=result[security,field],dateLastUpdate=Sys.time()))
			}
		}
		blpDisconnect(conn)
		# empty the request list
		blRequestHandler[["requests"]] <<- list()
		return(blData)
	}
	
	return(blRequestHandler)
}
