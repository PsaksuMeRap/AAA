# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_fundsDB <- function() {
	# i fondi e le relative informazioni sono
	# inseriti manualmente nel seguente data.frame

	globalEquity <- new("FundRecord",fundName="GLOBAL EQUITY",securityClass="Fondi_azionari",
			id="2742261CH",owner="pippo53")
	
	globalEconomy <- new("FundRecord",fundName="GLOBAL ECONOMY",securityClass="Fondi_azionari",
			id="11995588CH",owner="pippo210")
	
	fixedIncome <- new("FundRecord",fundName="OC FIXED INCOME",securityClass="Fondi_obbligazionari",
			id="2490099",owner="pippo76")
	
	multistrategy <- new("FundRecord",fundName="OC DYNAMIC MS",securityClass="Fondi_obbligazionari",
			id="LU0810450972",owner="pippo101")
	
	asymmetric <- new("FundRecord",fundName="OC ASYMMETRIC",securityClass="Fondi_azionari",
			id="LU0810451277",owner="pippo100")
	
	fundsDB <- new("FundDB",list(globalEquity,globalEconomy,fixedIncome,multistrategy,asymmetric))
	
	return(fundsDB)
}

