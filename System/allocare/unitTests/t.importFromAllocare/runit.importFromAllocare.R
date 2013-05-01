# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldImportFile <- function() {
	
	file = "./allocare/unitTests/data/portafogli_allocare.txt"
	data.df <- read.delim(file,header=TRUE,as.is=TRUE)
	data.df[["Tree"]] <- NULL
	data.df <- data.df[(data.df[["Asset.Category"]]!=""),]
	
	repositoryAllocareInvestmentType <- create_repositoryAllocareInvestementType()
	
	getRow <- function(i,df) { 
		x <- df[i,,drop=TRUE]
		
		ratingSP <- x[["Rating.Standard...Poors"]]
		ratingMoody <- x[["Rating.Moodys"]]
		
		allocarePosition <- list(
				Cliente=as.character(x[["Portfolio"]]),
				Strumento=x[["Investment.Type"]],
				Moneta=x[["Currency"]],
				Saldo=as.numeric(gsub("'","",x[["Nominal.Quantity"]])),
				NumeroValore=x[["Investment.Object.Identifier"]],
				Nome=x[["Investment.Object.Name"]],
				PrezzoMercato=as.numeric(gsub("'","",x[["Clean.Price"]])),
				ValoreMercatoLocalCurrency=as.numeric(gsub("'","",x[["Value"]])),
				ID_AAA=0,
				ID_strumento=repositoryAllocareInvestmentType$getId(x[["Investment.Type"]]),
				rating=ifelse(ratingSP=="" | ratingSP=="NR",ratingMoody,ratingSP)
		)
		
		return(allocarePosition)
	}

	result <- lapply(1:nrow(data.df),getRow,data.df)
	class(result) <- "allocarePositions"
}
