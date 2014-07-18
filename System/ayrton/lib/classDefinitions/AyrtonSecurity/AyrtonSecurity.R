# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Ayrton_Equity",contains="AyrtonPosition")

setClass("Ayrton_Bond",contains="AyrtonPosition")

setClass("Ayrton_Bond_floater",contains="AyrtonPosition")

setClass("Ayrton_Fondi_obbligazionari",contains="Ayrton_Bond")

setClass("Ayrton_Fondi_mercato_monetario",contains="AyrtonPosition")

setClass("Ayrton_Floating_rate_notes",contains="AyrtonPosition")

setClass("Ayrton_Anticipi_fissi",contains="AyrtonPosition")

setClass("Ayrton_Depositi_a_termine",contains="AyrtonPosition")

setClass("Ayrton_GROI",contains="AyrtonPosition")

setClass("Ayrton_Certificate_PLUS",contains="AyrtonPosition")

setClass("Ayrton_Obbligazioni_step-down",contains="AyrtonPosition")

setClass("Ayrton_Obbligazioni_convertibili",contains="AyrtonPosition")

setClass("Ayrton_Equity_Yield_Note",contains="AyrtonPosition")

setClass("Ayrton_Barrier_Reverse_Convertible",contains="AyrtonPosition")

setClass("Ayrton_Fondi_azionari",contains="AyrtonPosition")

setClass("Ayrton_Index_certificate",contains="AyrtonPosition")

setClass("Ayrton_Perles",contains="AyrtonPosition")

setClass("Ayrton_Basket_certificate",contains="AyrtonPosition")

setClass("Ayrton_Opzioni_su_azioni",contains="AyrtonPosition")

setClass("Ayrton_Opzioni_su_divise",contains="AyrtonPosition")

setClass("Ayrton_Opzioni_su_obbligazioni",contains="AyrtonPosition")

setClass("Ayrton_Metalli_preziosi",contains="AyrtonPosition")

setClass("Ayrton_FX_Forward",contains="AyrtonPosition")

setClass("Ayrton_TOREROs",contains="AyrtonPosition")

setClass("Ayrton_TOROs",contains="AyrtonPosition")

setClass("Ayrton_Fondi_immobiliari",contains="AyrtonPosition")

setClass("Ayrton_Fondi_misti",contains="AyrtonPosition")

setClass("Ayrton_Capital_protection_unit",contains="AyrtonPosition")

setClass("Ayrton_GOALs",contains="AyrtonPosition")

setClass("Ayrton_Fund_of_funds",contains="AyrtonPosition")

setClass("Ayrton_Diritti_aumento_capitale_azionario",contains="AyrtonPosition")

setClass("Ayrton_Unclassified",contains="AyrtonPosition")

setClass("Ayrton_Capital_protected_notes",contains="AyrtonPosition")

setClass("Ayrton_Call_Ratio_Certificate",contains="AyrtonPosition")

setClass("Ayrton_Certificati_convertibili",contains="AyrtonPosition")

setClass("Ayrton_Reverse_convertible",contains="AyrtonPosition")

setClass("Ayrton_Certificate",contains="AyrtonPosition")

setClass("Ayrton_Inflation_Note",contains="AyrtonPosition")

setClass("Ayrton_Spread_Protected_Note",contains="AyrtonPosition")

setClass("Ayrton_Swaps",contains="AyrtonPosition")

setClass("Ayrton_Conto_corrente",contains="AyrtonPosition")

setClass("Ayrton_Conto_corrente_fittizio",contains="AyrtonPosition")

setClass("Ayrton_Conto_metallo_oro",contains="AyrtonPosition")

setClass("Ayrton_Call_Geld",contains="AyrtonPosition")

setClass("Ayrton_Trigger_Equity_Yield_Note",contains="AyrtonPosition")

setClass("Ayrton_Callable_Yield_Notes",contains="AyrtonPosition")

setClass("Ayrton_Fondi_Hedge",contains="AyrtonPosition")

setClass("Ayrton_ETF_equity",contains="AyrtonPosition")

setClass("Ayrton_ETF_bond",contains="AyrtonPosition")

setClass("Ayrton_Fondi_commodities",contains="AyrtonPosition")

setClass("Ayrton_Fondi_absolute_return",contains="AyrtonPosition")

setClass("Ayrton_Strutturati_EQ",contains="AyrtonPosition")

setClass("Ayrton_Strutturati_FI",contains="AyrtonPosition")

setClass("Ayrton_Futures_EQ",contains="AyrtonPosition")

setClass("Ayrton_ETF_commodities_gold",contains="AyrtonPosition")

setClass("Ayrton_ETF_commodities_platinum",contains="AyrtonPosition")

setClass("Ayrton_Credit_linked_note",contains="AyrtonPosition")

setClass("Ayrton_Fund_private_equity",contains="AyrtonPosition")

#create_classDefinition <- function(instrument) {
#	string <- paste("setClass(\"Ayrton_",instrument,"\",contains=\"AyrtonPosition\")\n\n",sep="")
#	cat(string)
#}

#for (i in df[,2]) create_classDefinition(i)
