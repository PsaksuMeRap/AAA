# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("idFactory",def=function(origin,...) standardGeneric("idFactory"))

#setMethod("idFactory",signature(origin="AyrtonPosition"),
#		function(origin) {
			
			# available instruments at 2012-04-10
			
			## 1,Equity
			## 2,Bond
			## 3,Fondi_obbligazionari
			## 4,Fondi_mercato_monetario
			## 5,Floating_rate_notes
			## 6,Anticipi_fissi
			## 7,Depositi_a_termine
			## 8,GROI
			## 9,Certificate_PLUS
			## 10,Obbligazioni_step-down
			## 11,Obbligazioni_convertibili
			## 12,Equity_Yield_Note
			## 13,Barrier_Reverse_Convertible
			## 14,Fondi_azionari
			## 15,Index_certificate
			## 16,Perles
			## 17,Basket_certificate
			## 18,Opzioni_su_azioni
			## 19,Opzioni_su_divise
			## 20,Opzioni_su_obbligazioni
			## 21,Metalli_preziosi
			## 22,FX_Forward
			## 23,TOREROs
			## 24,TOROs
			## 25,Fondi_immobiliari
			## 26,Fondi_misti
			## 27,Capital_protection_unit
			## 28,GOALs
			## 29,Fund_of_funds
			## 30,Diritti_aumento_capitale_azionario
			## 31,Non_classificato
			## 32,Capital_protected_notes
			## 33,Call_Ratio_Certificate
			## 34,Certificati_convertibili
			## 35,Reverse_convertible
			## 36,Certificate
			## 37,Inflation_Note
			## 38,Spread_Protected_Note
			## 39,Swaps
			## 40,Conto_corrente
			## 41,Call_Geld
			## 42,Trigger_Equity_Yield_Note
			## 43,Callable_Yield_Notes
			## 44,Fondi_Hedge
			## 45,ETF_equity
			## 46,Fondi_commodities
			## 47,Fondi_absolute_return
			## 48,Strutturati_EQ
			## 49,Strutturati_FI
			## 50,Futures_EQ
			## 51,ETF_commodities
			## 52,Credit_linked_note
			## 53,Unclassified "
			

#			if (is.na(origin@ID_AAA)) {
#				# check for particular instruments: Conto_corrente, 
#				no_ID_AAA_instruments <- c(5,6,7,18,19,21,22,40,50)			
#				if (!is.element(origin@ID_strumento,no_ID_AAA_instruments)) {
#					string <- "The following instrument is not implemented or is incomplete:"
#					string <- paste(string,
#							"\n  ID_strumento: ",origin@ID_strumento,
#							"\n  ID_AAA: ",origin@ID_AAA,
#							"\n  Name: ",origin@Nome,sep="")
#					stop(string)
#				}
#				idAyrton <- new("IdAyrton",
#						idAAA=new("IdAAA_string",origin@Nome),
#						idStrumento=origin@ID_strumento)
#				return(idAyrton)
#			}
#			
#			idAyrton <- new("IdAyrton",
#					idAAA=new("IdAAA_numeric",origin@ID_AAA),
#					idStrumento=origin@ID_strumento)
#			
#			return(idAyrton)
#		}
#)

