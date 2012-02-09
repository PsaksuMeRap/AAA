# TODO: Add comment
# 
# Author: claudio
###############################################################################


# crea la classe AyrtonPosition uguale al record della tabella DBPortfolioGenerale
setClass("AyrtonPosition",representation(Cliente="character",Strumento="character",Moneta="character",
				Saldo="numeric",Nome="character",ValoreMercatoMonetaCHF="numeric",ID_AAA="numeric",
				ID_strumento="numeric"))

# crea la classe AyrtonPositions uguale all'insieme dei records della tabella DBPortfolioGenerale
setClass("AyrtonPositions",representation(positions="list"))