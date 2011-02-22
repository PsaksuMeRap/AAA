# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_importer <- function() {
	importer <- new.env()
	class(importer) <- "dataStreamImporter"
	
	importer$file <- "./unitTests/data/vixAndCo.csv"

	importer$readStartEndFrequency <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,nrows = 3)
		
		startDate = input[1,2]
		endDate = input[2,2]
		frequency = input[3,2]
		
		attributes <- list(startDate=startDate,endDate=endDate,
				frequency=frequency)
		return(attributes)
	}
	
	importer$readDsNames <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=3,nrows = 1)
		
		names = as.vector(input[1,-1],mode="character")
		return(names)
	}
	
	importer$readDsCodes <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=4,nrows = 1)
		
		codes = as.vector(input[1,-1],mode="character")
		
		return(codes)
	}
	
	importer$getData <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=5)
		
		dates <- input[,1]
		data <- input[,-1,drop=FALSE]
		rownames(data) <- dates
		
		colnames(data) <- importer$readDsCodes()
		return(data)
	}
	
	importer$createRepository <- function() {
		dsNames <- importer$readDsNames()
		dsCodes <- importer$readDsCodes()
		data <- importer$getData()
		
		repository <- list()
		class(repository) <- "repository"
		
		for (i in 1:length(dsCodes)) {
			repository[[i]] <- create_dsTimeseries(name=dsNames[i],
					dsCode=dsCodes[i], data=data[,i,drop=FALSE])
		} 
		return(repository)
	}
	
	return(importer)
	
}

