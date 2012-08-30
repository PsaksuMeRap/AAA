# TODO: Add comment
# 
# Author: claudio
###############################################################################
sysAyrton <- list()

# set the default string for the maturity of the Fondi_obbligazionari

Fondi_obbligazionari <- list(
	maturity="2020-12-31",
	interestRate="0%",
	maturityHorizon=">3Y",
	preNameString="20201231 - 0% >3Y"
)

sysAyrton[["Fondi_obbligazionari"]] <- Fondi_obbligazionari

rm(Fondi_obbligazionari)