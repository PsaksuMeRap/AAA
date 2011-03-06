# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_importer <- function(importFrom) {
	importer <- new.env()
	class(importer) <- "dsImporter"
	
	if (missing(importFrom)) {
		importer$file <- "./unitTests/data/vixAndCo.csv"
	} else {
		importer$file <- importFrom
	}
	
	importer$repository <- new.env()
	
	importer$readStartEndFrequency <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,nrows = 3,
				na.strings = "#NA")
		
		startDate = input[1,2]
		endDate = input[2,2]
		frequency = input[3,2]
		
		attributes <- list(startDate=startDate,endDate=endDate,
				frequency=frequency)
		return(attributes)
	}
	
	importer$readDsNames <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=3,nrows = 1,
				na.strings = "#NA")
		
		names = as.vector(input[1,-1],mode="character")
		importer$repository$dsNames <<- names
	}
	
	importer$readDsCodes <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=4,nrows = 1,
				na.strings = "#NA")
		
		codes = as.vector(input[1,-1],mode="character")
		
		importer$repository$dsCodes <<- codes
	}
	
	importer$getData <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=5,
				na.strings = "#NA")
		
		dates <- input[,1]
		data <- input[,-1,drop=FALSE]
		rownames(data) <- dates
		
		colnames(data) <- importer$readDsCodes()
		
		importer$repository$data <<- data
	}
	
	importer$createRepository <- function() {
		if (!exists("dsNames",where=importer$repository,inherits = FALSE)) {
			importer$readDsNames()
		}	
		dsNames <- importer$repository$dsNames
		
		if (!exists("dsCodes",where=importer$repository,inherits = FALSE)) {
			importer$readDsCodes()
		}	
		dsCodes <- importer$repository$dsCodes
		
		if (!exists("data",where=importer$repository,inherits = FALSE)) {
			importer$getData()
		}
		data <- importer$repository$data
		
		repository <- list()
		class(repository) <- "repository"
		dates <- rownames(data) 
				
		for (i in 1:length(dsCodes)) {
			x <- data[,i,drop=FALSE]
			rownames(x)<- dates
			repository[[i]] <- create_dsTimeseries(name=dsNames[i],
					dsCode=dsCodes[i], data=x)
		} 
		return(repository)
	}
	
	return(importer)

}

