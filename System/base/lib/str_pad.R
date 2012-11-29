# TODO: Add comment
# 
# Author: claudio
###############################################################################


str_pad <- function(x,width,side="right") {
	nbChar <- nchar(x)
	if (nbChar>=width) return(x)
	
	textToAdd <- paste(rep(" ",width - nbChar),collapse="")
	
	if (side=="right") return(paste(x,textToAdd,sep=""))
	
	return(paste(textToAdd,x,sep=""))
}
