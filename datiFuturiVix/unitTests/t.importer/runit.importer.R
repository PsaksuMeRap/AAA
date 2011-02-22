# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_importerVixFutures <- function() {
	
	importer <- create_importer()
	
	checkEquals(class(importer),"dataStreamImporter")
	
}

test.importer_readStartEndFrequency <- function() {
	# see runit.importVixFutures.R	
}

test.importer_readDsNames <- function() {
# see runit.importVixFutures.R	
}

test.importer_readDsCodes <- function() {
# see runit.importVixFutures.R	
}

test.importer_getData <- function() {
# see runit.importVixFutures.R	
}

test.importer_createRepository <- function() {
	importer <- create_importer()
	
	repository <- importer$createRepository() 
	
	checkEquals(length(repository),11)
}