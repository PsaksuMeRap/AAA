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
				return(invisible()) 
			} else {
				dir.create(file.path(x@absolutePath,"postOffice"))
				dir.create(file.path(x@absolutePath,"postOffice","inbox"))
				dir.create(file.path(x@absolutePath,"postOffice","pending"))
				return(invisible())
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
				if (file.exists(path)) return(invisible())
				dir.create(path)
				dir.create(file.path(path,"pending"))
				return(invisible())
			}
		}
)

lockMailBox <- function(mailBox,PostOffice) {
	# verify that the postOffice exists
	advisorEmail <- mailBox@advisor@email
	directoriesInPostOffice <- dir(file.path(PostOffice@absolutePath,"postOffice"))
	if (!is.element("inbox",directoriesInPostOffice)) {
		message <- paste("The postOffice",file.path(PostOffice@absolutePath,"postOffice"), "is not available.\n")
		message <- paste(message," Impossible to lock the mailBox of ",advisorEmail,sep="")
		stop(message)
	}
	directoriesInMailBox <- dir(file.path(PostOffice@absolutePath,"postOffice",mailBox@advisor@folderName))
	if (!is.element("inbox",directoriesInMailBox)) {
		message <- paste("The mailBox of",advisorEmail, "is not available.\n")
		message <- paste(message," Impossible to lock the mailBox of ",advisorEmail,".",sep="")
		stop(message)
	}
	
	path <- file.path(PostOffice@absolutePath,"postOffice",mailBox@advisor@folderName)
	# check that the lock is not still in place
	stillLocked <- file.exists(file.path(path,"lock"))

	# lock the mailBox if necessary
	if (!stillLocked) { 
		created <- file.create(file.path(path,"lock"),showWarnings=FALSE)
		if (!created) {
			message <- paste("Impossible to create the lock for the mailBox of ",advisorEmail,"\n",sep="")
			message <- paste(message,"Unrecoverable error. Procedure stopped here!")
			stop(message)
		} else {
			return(TRUE)
		}
	} else {
		return(FALSE)
	}
	
}
