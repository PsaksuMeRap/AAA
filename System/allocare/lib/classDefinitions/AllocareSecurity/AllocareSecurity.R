# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Allocare_Equity",contains="AllocarePosition")

setClass("Allocare_Bond",contains="AllocarePosition")

setClass("Allocare_Bond_floater",contains="AllocarePosition")

setClass("Allocare_Fondi_obbligazionari",contains="Allocare_Bond")

setClass("Allocare_Fondi_mercato_monetario",contains="AllocarePosition")

setClass("Allocare_Floating_rate_notes",contains="AllocarePosition")

setClass("Allocare_Anticipi_fissi",contains="AllocarePosition")

setClass("Allocare_Depositi_a_termine",contains="AllocarePosition")

setClass("Allocare_GROI",contains="AllocarePosition")

setClass("Allocare_Certificate_PLUS",contains="AllocarePosition")

setClass("Allocare_Obbligazioni_step-down",contains="AllocarePosition")

setClass("Allocare_Obbligazioni_convertibili",contains="AllocarePosition")

setClass("Allocare_Equity_Yield_Note",contains="AllocarePosition")

setClass("Allocare_Barrier_Reverse_Convertible",contains="AllocarePosition")

setClass("Allocare_Fondi_azionari",contains="AllocarePosition")

setClass("Allocare_Index_certificate",contains="AllocarePosition")

setClass("Allocare_Perles",contains="AllocarePosition")

setClass("Allocare_Basket_certificate",contains="AllocarePosition")

setClass("Allocare_Opzioni_su_azioni",contains="AllocarePosition")

setClass("Allocare_Opzioni_su_divise",contains="AllocarePosition")

setClass("Allocare_Opzioni_su_obbligazioni",contains="AllocarePosition")

setClass("Allocare_Metalli_preziosi",contains="AllocarePosition")

setClass("Allocare_FX_Forward",contains="AllocarePosition")

setClass("Allocare_TOREROs",contains="AllocarePosition")

setClass("Allocare_TOROs",contains="AllocarePosition")

setClass("Allocare_Fondi_immobiliari",contains="AllocarePosition")

setClass("Allocare_Fondi_misti",contains="AllocarePosition")

setClass("Allocare_Capital_protection_unit",contains="AllocarePosition")

setClass("Allocare_GOALs",contains="AllocarePosition")

setClass("Allocare_Fund_of_funds",contains="AllocarePosition")

setClass("Allocare_Diritti_aumento_capitale_azionario",contains="AllocarePosition")

setClass("Allocare_Unclassified",contains="AllocarePosition")

setClass("Allocare_Capital_protected_notes",contains="AllocarePosition")

setClass("Allocare_Call_Ratio_Certificate",contains="AllocarePosition")

setClass("Allocare_Certificati_convertibili",contains="AllocarePosition")

setClass("Allocare_Reverse_convertible",contains="AllocarePosition")

setClass("Allocare_Certificate",contains="AllocarePosition")

setClass("Allocare_Inflation_Note",contains="AllocarePosition")

setClass("Allocare_Spread_Protected_Note",contains="AllocarePosition")

setClass("Allocare_Swaps",contains="AllocarePosition")

setClass("Allocare_Conto_corrente",contains="AllocarePosition")

setClass("Allocare_Conto_corrente_fittizio",contains="AllocarePosition")

setClass("Allocare_Conto_metallo_oro",contains="AllocarePosition")

setClass("Allocare_Call_Geld",contains="AllocarePosition")

setClass("Allocare_Trigger_Equity_Yield_Note",contains="AllocarePosition")

setClass("Allocare_Callable_Yield_Notes",contains="AllocarePosition")

setClass("Allocare_Fondi_Hedge",contains="AllocarePosition")

setClass("Allocare_ETF_equity",contains="AllocarePosition")

setClass("Allocare_Fondi_commodities",contains="AllocarePosition")

setClass("Allocare_Fondi_absolute_return",contains="AllocarePosition")

setClass("Allocare_Strutturati_EQ",contains="AllocarePosition")

setClass("Allocare_Strutturati_FI",contains="AllocarePosition")

setClass("Allocare_Futures_EQ",contains="AllocarePosition")

setClass("Allocare_ETF_commodities_gold",contains="AllocarePosition")

setClass("Allocare_ETF_commodities_platinum",contains="AllocarePosition")

setClass("Allocare_Credit_linked_note",contains="AllocarePosition")

setClass("Allocare_Fund_private_equity",contains="AllocarePosition")

