# TODO: Add comment
# 
# Author: claudio
###############################################################################


createOriginData <- function() {
	getRow <- function(i,df) { 
		x <- df[i,,drop=TRUE]
		ayrtonPosition <- new("AyrtonPosition",
				Cliente=x[["Cliente"]],Strumento=x[["Strumento"]],Moneta=x[["Moneta"]],
				Saldo=x[["Saldo"]],Nome=x[["Nome"]],ValoreMercatoMonetaCHF=x[["ValoreMercatoMonetaCHF"]],
				ID_AAA=as.numeric(x[["ID_AAA"]]),ID_strumento=x[["ID_strumento"]])
		return(ayrtonPosition)
	}
	
	dati.df <- read.csv("./base/unitTests/data/origin.csv",
			header=TRUE,stringsAsFactors=FALSE)
	
	if (nrow(dati.df)==0) return(origin=new("AyrtonPositions",positions=list()))
	origin <- lapply(1:nrow(dati.df),getRow,dati.df)
	return(origin)
}
