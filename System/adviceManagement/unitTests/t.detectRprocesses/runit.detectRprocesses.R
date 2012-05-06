# TODO: Add comment
# 
# Author: ortellic
###############################################################################

# deactivate once ok
test.shouldGetNoPID <- function() {
	if(.Platform$OS.type=="windows") {
		
		imageName <- "Rgui"	
		result <- get_PID(imageName)
		checkEquals(result,numeric(0))
	
	} else {
		
		programName <- "R"
		result <- system("pidof R",intern=FALSE,wait=TRUE)
		checkEquals(result,1)
		
	}

}

# deactivate once ok
test.shouldGetEclipsePID <- function() {
	if(.Platform$OS.type=="windows") {
		imageName <- "eclipse.exe"
		
		result <- get_PID(imageName)
		
		# should must be updated every time the eclipse environment is restarted
		# or this test function must be disabled
		should <- 2252
		checkEquals(result,should)
	} else {
		programName <- "R"
		result <- system("pidof R",intern=FALSE,wait=TRUE)
		checkEquals(result,1)
	}
	

	
}

test.shouldGetNewRguiPID <- function() {
	
	imageName <- "Rgui.exe"
	# launch new Rgui.exe process
	system("Rgui.exe",invisible=FALSE,intern=FALSE,wait=FALSE)
	
	result <- get_PID(imageName)
	# should must be updated every time the eclipse environment is restarted
	# or this test function must be disabled
	should <- 5240
	checkEquals(result,should)
	
}

test.shouldGetTwoNewRguiPID <- function() {
	
	imageName <- "Rgui.exe"

	# launch new Rgui.exe process
	system("Rgui.exe",invisible=FALSE,intern=FALSE,wait=FALSE)
		
	result <- get_PID(imageName)
	# should must be updated every time the eclipse environment is restarted
	# or this test function must be disabled
	should <- c(5240,1228)
	checkEquals(result,should)
	
	for (pid in result) {
		isKilled <- system(paste("taskkill /F /PID",pid),intern=TRUE,wait=TRUE)
	}
}


test.shouldIdentifyTheNewRguiProcess <- function() {
	
	imageName <- "Rgui.exe"
	existing_PIDs <- c(5240,1228)

	# launch new Rgui.exe process
	system("Rgui.exe",invisible=FALSE,intern=FALSE,wait=FALSE)
	PIDs <- get_PID(imageName)
	
	result <- getNew_PID(PIDs,existing_PIDs)
	# should must be updated every time the eclipse environment is restarted
	# or this test function must be disabled
	should <- 3428
	checkEquals(result,should)
	
	for (pid in PIDs) {
		isKilled <- system(paste("taskkill /F /PID",pid),intern=TRUE,wait=TRUE)
	}
}