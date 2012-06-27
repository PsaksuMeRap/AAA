# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("PostOffice",representation(absolutePath="character",mailBoxes="list"),
		prototype=list(inbox=character(0),pending=character(0)))


setGeneric("setup",def=function(x,y,...) standardGeneric("setup"))


setMethod("setup",signature(x="PostOffice",y="missing"),
		definition=function(x) {

			# verify that the postOffice does not exists
			listOfDirectories <- dir(path=x@absolutePath)
			if (is.element("postOffice",listOfDirectories)) {
				# we then assume that the postOffice has still been created
				return(invisible()) 
			} else {
				dir.create(file.path(x@absolutePath,"postOffice"))
				dir.create(file.path(x@absolutePath,"postOffice","inbox"))
				# dir.create(file.path(x@absolutePath,"postOffice","pending"))
				return(invisible())
			}
		}
)


