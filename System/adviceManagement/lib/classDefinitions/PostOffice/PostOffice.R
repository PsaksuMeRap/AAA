# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("MailBox",representation(inbox="character",outbox="character",sent="character",advisor="Advisor"),
		prototype=list(inbox=character(0),outbox=character(0),sent="character(0)"))

setClass("PostOffice",representation(absolutePath="character",mailBoxes="list"))


setGeneric("setup",def=function(x,y,...) standardGeneric("setup"))


setMethod("setup",signature(x="PostOffice",y="missing"),
		definition=function(x) {
			# verify that the postOffice does not exists
			listOfDirectories <- dir(path=x@absolutePath)
			if (is.element("postOffice",listOfDirectories)) {
				# we then assume that the postOffice has still been created
				return() 
			} else {
				dir.create(file.path(x@absolutePath,"postOffice"))
				dir.create(file.path(x@absolutePath,"postOffice","inbox"))
				dir.create(file.path(x@absolutePath,"postOffice","outbox"))
				dir.create(file.path(x@absolutePath,"postOffice","sent"))
				return()
			}
		}
)


setMethod("setup",signature(x="MailBox",y="PostOffice"),
		definition=function(x,y) {
			# verify that the postOffice exists
			directoriesInPostOffice <- dir(file.path(y@absolutePath,"postOffice"))
			if (!is.element("inbox",directoriesInPostOffice)) {
				message <- paste("The postOffice",y@absolutePath, "is not available")
				stop(message)
			} else {
				# create the necessary directories
				path <- file.path(y@absolutePath,"postOffice",x@advisor@folderName)
				dir.create(path)
				dir.create(file.path(path,"inbox"))
				dir.create(file.path(path,"outbox"))
				dir.create(file.path(path,"sent"))
				return()
			}
		}
)

