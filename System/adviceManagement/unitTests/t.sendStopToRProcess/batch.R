# TODO: Add comment
# 
# Author: claudio
############cat("No stop. Wait 5 seconds more ... ",append=TRUE,file="logFile")###################################################################

continue <- TRUE
logFile <- file(description="logFile",open="w")
cat(getwd(),sep="\n",append=TRUE,file=logFile)
	x <- 1
while(continue) {
	Sys.sleep(x)
	continue <- !file.exists("stop")
	if (continue) cat(paste("No stop. Wait",x,"seconds more ... "),sep="\n",append=TRUE,file=logFile)
}

cat(paste(Sys.time(),"Stop current R session! "),sep="\n",append=TRUE,file=logFile)
