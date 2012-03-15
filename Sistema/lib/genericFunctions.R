# TODO: Add comment
# 
# Author: claudio
###############################################################################


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
