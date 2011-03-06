# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_importer <- function() {
	
	importer <- create_importer()
	
	checkEquals(class(importer),"dsImporter")
	
}

test.importer_readStartEndFrequency <- function() {
	
	importer <- create_importer()
	attributes <- importer$readStartEndFrequency() 
	checkEquals(attributes$startDate,"1998-01-01")
	checkEquals(attributes$endDate,"2009-10-13")
	checkEquals(attributes$frequency,"D")
}

test.importer_readDsNames <- function() {
	importer <- create_importer()
	
	if (!exists("dsNames",where=importer$repository,inherits = FALSE)) {
		importer$readDsNames()
	}	
	names <- importer$repository$dsNames
	
	checkEquals(names[2],"VDAX-NEW VOLATILITY INDEX - PRICE INDEX")
}

test.importer_readDsCodes <- function() {
	importer <- create_importer()
	codes <- importer$readDsCodes() 
	
	checkEquals(codes[3],"VSTOXXI")
}

test.importer_getData <- function() {
	importer <- create_importer()
	data <- importer$getData()
	
	checkEquals(rownames(data)[1],"1998-01-01")
	checkEquals(rownames(data)[nrow(data)],"2011-02-21")
	checkEquals(nrow(data),3428)
	checkEquals(data[1,1],24.01)
}

test.importer_getDataWith_NA <- function() {
	importer <- create_importer(importFrom="./unitTests/data/serie storiche datastream.csv")
	data <- importer$getData()

print(data[3,9])
	checkEquals(data[3,9],NA_real_)
	checkEquals(data[3,18],NA_real_)	
}


test.importer_createRepository <- function() {
	importer <- create_importer()
	
	repository <- importer$createRepository() 
	
	checkEquals(length(repository),11)
}