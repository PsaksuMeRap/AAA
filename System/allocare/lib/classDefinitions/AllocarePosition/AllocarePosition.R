# TODO: Add comment
# 
# Author: claudio
###############################################################################


# crea la classe AllocarePosition uguale al record del file di importazione
setClass("AllocarePosition",
		representation(Cliente="character",
				Strumento="character",Moneta="character",
				Saldo="numeric",NumeroValore="character",
				Nome="character",PrezzoMercato="numeric",
				ValoreMercatoLocalCurrency="numeric",ID_AAA="numeric",
				ID_strumento="numeric",rating="character",
				EconomicExposure="numeric",Underlying="character",
				UnderlyingQuantity="numeric",MaturityDate="character",
				OptionFeature="character"
		)
)


