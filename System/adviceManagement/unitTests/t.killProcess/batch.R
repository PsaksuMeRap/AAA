# TODO: Add comment
# 
# Author: claudio
############cat("No stop. Wait 5 seconds more ... ",append=TRUE,file="logFile")###################################################################
print(getwd())
#directory <- file.path(getwd(),"unitTests","t.killProcess")
#directory <- file.path(getwd(),"adviceManagement","unitTests","t.killProcess")
fileName <- "stop"#file.path(directory,"stop")
#setwd(directory)
directory <- getwd()
if (getwd()==directory) {
	continue <- TRUE
	logFile <- file(description="logFile",open="w")
	while(continue) {
		Sys.sleep(5)
		continue <- !file.exists("stop")
		cat("No stop. Wait 5 seconds more ... ",sep="\n",append=TRUE,file=logFile)
	}
	cat("Stop current R session! ",sep="\n",append=TRUE,file=logFile)
	quit(save="no")
} else {
	print(getwd())
	quit(save="no")
}