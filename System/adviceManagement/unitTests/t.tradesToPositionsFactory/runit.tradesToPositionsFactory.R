# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreatePositionsFromTrades <- function() {
	
	# copy the data directory
	dirToCopy <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(sys[["homeDir"]])
	file.copy(from=dirToCopy,to=to,recursive=TRUE)
	
	# create the BloombergData

	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	assign("bloombergData",createRepositoryBloombergData(),pos=repositories)
	
	# set the fileName from which to import trades
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradesToPositionsFactory") 
	
	positions <- tradesToPositionsFactory(fileName,directory)
	
	checkEquals(length(positions),6)
	checkEquals(as.character(positions[[3]]@security@name),"UBS AG-REG")
	
	rm("bloombergData",pos=repositories)
	unlink(file.path(sys[["homeDir"]],"data"),recursive=TRUE)
}

test.shouldCreateLongPositionsFromTrades <- function() {
	
	# copy the data directory
	dirToCopy <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(sys[["homeDir"]])
	file.copy(from=dirToCopy,to=to,recursive=TRUE)
	
	# create the BloombergData
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	assign("bloombergData",createRepositoryBloombergData(),pos=repositories)
	
	# set the fileName from which to import trades
	fileName <- "2012-06-19_14-27-48_Ortelli_globalEconomy_newAdvice.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradesToPositionsFactory") 
	
	positions <- tradesToPositionsFactory(fileName,directory)
	
	checkEquals(length(positions),18)
	checkEquals(as.character(positions[[1]]@security@name),"UBS AG-REG")
	
	rm("bloombergData",pos=repositories)
	unlink(file.path(sys[["homeDir"]],"data"),recursive=TRUE)
}

test.shouldCreateEmptyPositionsFromEmptyTrades <- function() {
	
	# copy the data directory
	dirToCopy <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(sys[["homeDir"]])
	file.copy(from=dirToCopy,to=to,recursive=TRUE)
	
	# create the BloombergData
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	assign("bloombergData",createRepositoryBloombergData(),pos=repositories)
	
	# set the fileName from which to import trades
	fileName <- "2012-06-19_14-27-49_Ortelli_globalEconomy_newAdvice.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradesToPositionsFactory") 
	
	positions <- tradesToPositionsFactory(fileName,directory)

	checkEquals(length(positions),0)
	
	rm("bloombergData",pos=repositories)
	unlink(file.path(sys[["homeDir"]],"data"),recursive=TRUE)
}
