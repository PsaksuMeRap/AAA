# TODO: Add comment
# 
# Author: Claudio
###############################################################################


logger <- function(message,processName="main") {
	cat(paste(Sys.time()," from ",processName,": ",message,"\n",sep=""))
}

