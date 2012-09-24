setGeneric("idFactory",def=function(origin,...) standardGeneric("idFactory"))

setMethod("idFactory",signature(origin="AyrtonPosition"),
		function(origin) {
			
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
			## 53,Unclassified
			## 54,Conto_corrente_fittizio"
			
			instrumentsWithISIN <- c(1,2,3,4,5,8:17,23:30,32:38,42:52,54)
			if (is.element(origin@ID_strumento,instrumentsWithISIN)) {
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",origin@NumeroValore),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Anticipi_fissi and Depositi_a_termine
			if (is.element(origin@ID_strumento,c(6,7))) {
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",origin@Nome),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Opzioni_su_azioni
			if (origin@ID_strumento==18) {
				parseOptionOnEquityName <- function(name) {
					tmp <- strsplit(name,"/")[[1]]
					tmp <- str_trim(tmp)
					names(tmp) <- c("numberEquities","callPut","underlyingName","maturity","strike","premium","ISIN","underlyingPrice")
					tmp <- as.list(tmp)
					tmp[["numberEquities"]] <- as.numeric(tmp[["numberEquities"]])
					if (tmp[["callPut"]]=="Call") tmp[["callPut"]] <- "C" else tmp[["callPut"]] <- "P"
					
					tmp[["maturity"]] <- format(strptime(tmp[["maturity"]],format="%d-%m-%y"),"%d-%m-%Y")
					tmp[["strike"]] <- as.numeric(substr(tmp[["strike"]],8,nchar(tmp[["strike"]])))
					tmp[["underlyingPrice"]] <- as.numeric(tmp[["underlyingPrice"]])
					#'-1000 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90'
					return(tmp)
				}
				parsedName <- parseOptionOnEquityName(origin@Nome)
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",parsedName[["ISIN"]]),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			
		}
)

		

