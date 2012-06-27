# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("MailBox",representation(name="character",folderName="character"))

setMethod("setup",signature(x="MailBox",y="PostOffice"),
		definition=function(x,y) {
			# verify that the postOffice exists
			directoriesInPostOffice <- dir(file.path(y@absolutePath,"postOffice"))
			if (!is.element("inbox",directoriesInPostOffice)) {
				message <- paste("The postOffice",y@absolutePath, "is not available")
				stop(message)
			} else {
				# create the necessary directories
				path <- file.path(y@absolutePath,"postOffice",x@folderName)
				if (file.exists(path)) return(invisible())
				dir.create(path)
				dir.create(file.path(path,"pending"))
				return(invisible())
			}
		}
)

lockMailBox <- function(mailBox,PostOffice) {
	# verify that the postOffice exists
	
	postOfficeExists <- file.exists(file.path(PostOffice@absolutePath,"postOffice"))
	if (!postOfficeExists) {
		message <- paste("The postOffice",file.path(PostOffice@absolutePath,"postOffice"), "is not available.\n")
		message <- paste(message," Impossible to lock the mailBox of ",mailBox@name,sep="")
		stop(message)
	}
	directoriesInPostOffice <- dir(file.path(PostOffice@absolutePath,"postOffice"))
	if (!is.element(mailBox@folderName,directoriesInPostOffice)) {
		message <- paste("The mailBox of",mailBox@name, "is not available.\n")
		message <- paste(message," Impossible to lock the mailBox of ",mailBox@name,".",sep="")
		stop(message)
	}
	
	path <- file.path(PostOffice@absolutePath,"postOffice",mailBox@folderName)
	# check that the lock is not still in place
	stillLocked <- file.exists(file.path(path,"lock"))
	
	# lock the mailBox if necessary
	if (stillLocked) {
		# the mailBox is still locked, impossible to lock a second time
		return(FALSE)
	} else {
		created <- file.create(file.path(path,"lock"),showWarnings=FALSE)
		if (!created) {
			message <- paste("Impossible to create the lock for the mailBox of ",mailBox@name,"\n",sep="")
			message <- paste(message,"Unrecoverable error. Procedure stopped here!")
			stop(message)
		} else {
			return(TRUE)
		}
	}
	
}

