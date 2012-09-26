# TODO: Add comment
# 
# Author: claudio
###############################################################################

#groupBySecurityId is a method used to join two positions on the same instrument.
#equality of the positions is determined by the security@id field
setGeneric("groupBySecurityId",function(x,y,...) standardGeneric("groupBySecurityId"))

# print is still a generic function
fun <- function(x) standardGeneric("print")
setGeneric("print", fun)

# join is a generic function used to join Positions or Portfolios
# the positions are joined and duplicated positions are not removed
# or summed together
setGeneric("join",function(x,y,...) standardGeneric("join"))

removeStartEndSpaces <- function(string) {
	result <- sub("^[[:blank:]]+", "", string)
	result <- sub("[[:blank:]]+$", "", result)
	return(result)
}

mySetwd <- function(directory) {
	#if(.Platform$OS.type=="windows") {
		#home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
	#} else {
	#	home <- "/home/claudio/workspace/AAA/System/"
	#}
	if(missing(directory)) {
		setwd(sys[["sourceCodeDir"]])
		path <- sys[["sourceCodeDir"]]
	} else {
		path <- file.path(sys[["sourceCodeDir"]],directory)
		setwd(path)
	}
	return(invisible(path))
}
