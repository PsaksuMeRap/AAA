# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./odbc/connessioni.R")

clienteFI <- "pippo76" # Euro fixed income
clienteGec <- "pippo210" # Global economy
clienteGeq <- "pippo53" # Global equity

fetchDate <- "2011-12-30"
desiredFields <- c("Cliente",
		"Strumento",
		"Moneta",
		"Nome",
		"Categoria",
		"ValoreMercatoMonetaCHF",
		"ID_AAA","ID_strumento",
		"Scadenza")

datiFI <- importDBPortfolioGeneraleDataFrame(c=clienteFI,f=fetchDate)[,desiredFields]
datiGec <- importDBPortfolioGeneraleDataFrame(c=clienteGec,f=fetchDate)[,desiredFields]
datiGeq <- importDBPortfolioGeneraleDataFrame(c=clienteGeq,f=fetchDate)[,desiredFields]

# prendi repositoryAssetClassMapping
repo <- create_repositoryAssetClassMapping()
missing <- datiFI[-(1:nrow(datiFI)),]

for (i in 1:nrow(datiFI)) {
	isElement <- datiFI[i,"Nome"] == repo$assetClassMapping.df[["Nome"]]
	if (!any(isElement)) missing <- rbind(missing,datiFI[i,"Nome"])
}


