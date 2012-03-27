# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_fundsDB <- function() {
	# i fondi e le relative informazioni sono
	# inseriti manualmente nel seguente data.frame

	globalEquity <- new("FundRecord",fundName="GLOBAL EQUITY",securityClass="Fondi_azionari",
			id=1701,owner="pippo53")
	globalEconomy <- new("FundRecord",fundName="GLOBAL ECONOMY",securityClass="Fondi_azionari",
			id=2256,owner="pippo210")
	fixedIncome <- new("FundRecord",fundName="FIXED INCOME",securityClass="Fondi_obbligazionari",
			id=825,owner="pippo76")
	
	fundsDB <- new("FundDB",list(globalEquity,globalEconomy,fixedIncome))
	
	return(fundsDB)
}

