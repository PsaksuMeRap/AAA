# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldGetNoPID <- function() {
	imageName <- "R"
	if(.Platform$OS.type=="windows") {
		result <- get_PID(imageName)
	} else {
		result <- get_PID(imageName)
	}
	
	checkEquals(result,numeric(0))
}


test.shouldGetEclipsePID <- function() {
	# "should" should be updated every time the eclipse environment is restarted
	# for this reason we don't test exact equality
	DEACTIVATED("Deactivated test because no eclipse process when lounching eclipse from icon")
	if(.Platform$OS.type=="windows") {
		imageName <- "eclipse.exe"
	} else {
		imageName <- "eclipse"
	}
	
	result <- get_PID(imageName)
	should <- 2252
	checkEquals(length(result),1)
	checkEquals(is.numeric(result),TRUE)
}

test.shouldGetNewRguiPID <- function() {
	if(.Platform$OS.type=="windows") {
		imageName <- "Rgui.exe"
		# launch new Rgui.exe process
		system(imageName,invisible=FALSE,intern=FALSE,wait=FALSE)
		
		result <- get_PID(imageName)
		
		# "should" should be updated every time the eclipse environment is restarted
		# for this reason we don't test exact equality
		should <- 5240
		checkEquals(length(result),1)
		checkEquals(is.numeric(result),TRUE)
	} else {
		checkEquals(1,1)
	}
}

test.shouldGetTwoNewRguiPID <- function() {
	if(.Platform$OS.type=="windows") {
		imageName <- "Rgui.exe"
		
		# launch new Rgui.exe process
		system(imageName,invisible=FALSE,intern=FALSE,wait=FALSE)
		
		result <- get_PID(imageName)
		
		# "should" should be updated every time the eclipse environment is restarted
		# for this reason we don't test exact equality
		should <- c(5240,1228)
		checkEquals(length(result),2)
		checkEquals(is.numeric(result),TRUE)
		
		for (pid in result) {
			isKilled <- system(paste("taskkill /F /PID",pid),intern=TRUE,wait=TRUE)
		}
	} else {
		checkEquals(1,1)
	}
}


test.shouldIdentifyTheNewRguiProcess <- function() {
	
	existing_PIDs <- c(5240,1228)

	PID <- c(123,5240,33,1228,12345)
	
	result <- getNew_PIDs(PID,existing_PIDs)

	should <- c(123,33,12345)
	checkEquals(result,should)
	
}