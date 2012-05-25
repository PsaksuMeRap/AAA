# TODO: Add comment
# 
# Author: ortellic
###############################################################################


# create the class containing the numeric or character data type
setClassUnion("BloombergFieldType",c("numeric","character"))
# create the class containing the value of the corresponding field and the date of last update
setClass("BloombergDataEntry",representation(blId="character",fieldId="character",value="BloombergFieldType",dateLastUpdate="POSIXct"))

# create the class containing the data of all securities
setClass("BloombergData",contains="namedList")

setGeneric("add",def=function(x,y,...) standardGeneric("add"))

setMethod("add",signature("BloombergDataEntry","BloombergData"),
		function(x,y) {
			# y: a BloombergData
			# x: a BloombergDataEntry
			y[[paste(x@blId,x@fieldId,sep="__")]] <- x
			return(y)
		}
)


setMethod("add",signature("BloombergDataEntry","BloombergDataEntry"),
		function(x,y) {
			# x: a BloombergDataEntry
			# y: a BloombergDataEntry
			
			blData <- new("BloombergData")
			blData <- add(x,blData)
			blData <- add(y,blData)
			
			return(blData)
		}
)

setMethod("add",signature("BloombergData","BloombergData"),
		function(x,y) {
			# x: a BloombergData
			# y: a BloombergData
			
			# 1: identify common elements
			
			# 2: if disjoint sets, join them and return
			
			# 3: if not disjoint, remove common elements from x and
			#    copy elements of y into x
			
			if (length(y)==0) return(x)
			
			xNames <- names(x)
			yNames <- names(y)
			
			common <- intersect(xNames,yNames)
			
			if (length(common)!=0) x[common] <- NULL
			x <- c(x,y)

			return(x)
		}
)

setMethod("as.character",signature("BloombergDataEntry"),
		function(x) {
			# x: a BloombergData
						
			string <- paste(x@blId,x@fieldId,x@value,x@dateLastUpdate,sep=" / ")
			return(string)
		}
)