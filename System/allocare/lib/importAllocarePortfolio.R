# TODO: Add comment
# 
# Author: claudio
###############################################################################


importAllocarePortfolio <- function(file) {
	
	data.df <- read.delim(file,header=TRUE,as.is=TRUE)
	
	if (nrow(data.df)==0) return(new("AllocarePositions"))
	
	data.df[["Tree"]] <- NULL
	data.df <- data.df[(data.df[["Asset.Category"]]!=""),]
	
	getRow <- function(i,df) { 
		x <- df[i,,drop=TRUE]
		
		ratingSP <- x[["Rating.Standard...Poors"]]
		ratingMoody <- x[["Rating.Moodys"]]
		
		allocarePosition <- new("AllocarePosition",
				Cliente=as.character(x[["Portfolio"]]),
				Strumento=x[["Investment.Type"]],
				Moneta=x[["Currency"]],
				Saldo=as.numeric(gsub("'","",x[["Nominal.Quantity"]])),
				NumeroValore=x[["Investment.Object.Identifier"]],
				Nome=x[["Investment.Object.Name"]],
				PrezzoMercato=as.numeric(gsub("'","",x[["Clean.Price"]])),
				ValoreMercatoLocalCurrency=as.numeric(gsub("'","",x[["Value"]])),
				ID_AAA=0,
				ID_strumento=repositories$allocareInvestmentType$getId(x[["Investment.Type"]]),
				rating=ifelse(ratingSP=="" | ratingSP=="NR",ratingMoody,ratingSP),
				EconomicExposure=as.numeric(gsub("'","",x[["Eco.Exp"]])),
				Underlying=x[["Underlying"]],
				UnderlyingQuantity=as.numeric(gsub("'","",x[["Underlying.Quantity"]])),
				OptionFeature=x[["Option.Feature"]]
		
		)
	}
	result <- new("AllocarePositions",lapply(1:nrow(data.df),getRow,data.df))
	return(result)
}
