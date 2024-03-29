setGeneric("idFactory",def=function(origin,...) standardGeneric("idFactory"))

setMethod("idFactory",signature(origin="AyrtonPosition"),
		function(origin) {
			
			# available instruments at 2015-04-15
			
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
			## 31,Unclassified
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
			## 51,ETF_commodities_gold
			## 52,Credit_linked_note
			## 53,Unclassified
			## 54,Conto_corrente_fittizio
			## 55,ETF_commodities_platinum
			## 56,Bond_floater
			## 58,ETF_bond
			## 59,Fondi_alternativi
			## 60,Strutturati_FX
			
			instrumentsWithISIN <- c(1,2,3,4,5,8:17,23:30,32:38,42:49,51:53,55,57,58,59,60)
			if (is.element(origin@ID_strumento,instrumentsWithISIN)) {
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",origin@NumeroValore),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Bond_floater
			if (origin@ID_strumento==56) {
				isin <- substr(origin@NumeroValore,1,12)
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",isin),
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
			# consider Conto_corrente and conto_corrente_fittizio
			if (is.element(origin@ID_strumento,c(40,54))) {
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",paste(origin@Moneta,tolower(origin@Moneta),sep="-")),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Opzioni_su_azioni
			if (origin@ID_strumento==18) {
				info <- getOptionParameters(origin)
				
				idAAA <- paste(info[["optionType"]],
						info[["isin"]],
						info[["expiryDate"]],
						info[["strike"]],
						sep="/"
					)
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",idAAA),
						idStrumento=origin@ID_strumento
					)
				
				return(idAyrton)
			}
			# consider Opzioni_su_divise
			if (origin@ID_strumento==19) {
				info <- getOptionParameters(origin)
				
				idAAA <- paste(info[["optionType"]],
							paste(info[["underlying"]],info[["numeraire"]],sep=""),
							info[["expiryDate"]],
							info[["strike"]],
							sep="/"
						)
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",idAAA),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Opzioni_su_obbligazioni
			if (origin@ID_strumento==20) {

				info <- getOptionParameters(origin)
				idAAA <- paste(info[["optionType"]],
							info[["isin"]],
							info[["expiryDate"]],
							info[["strike"]],
							sep="/"
						)
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",idAAA),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Metalli_preziosi
			if (origin@ID_strumento==21) {
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",origin@Nome),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Futures_EQ
			if (origin@ID_strumento==50) {
				parseFuturesEquityName <- function(name) {

					tmp1 <- strsplit(name,"/")[[1]]
					tmp1 <- remSpaces(tmp1)
					partToBeParsed <- tmp1[[1]]
					maturity <- substr(partToBeParsed,nchar(partToBeParsed)-9,nchar(partToBeParsed))
					maturity <- format(strptime(maturity,format="%d-%m-%Y"),"%Y-%m-%d")
					futureName <- substr(partToBeParsed,1,nchar(partToBeParsed)-11)
					futureName <- remSpaces(futureName)
					
					#"Future SMI 16-03-2012 / 10              "
					return(c(name=futureName,maturity=maturity))
				}
				parsedFutureName <- parseFuturesEquityName(origin@Nome)
				
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",paste(parsedFutureName[["name"]],parsedFutureName[["maturity"]],sep="")),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}			
			# consider FX_forward
			if (origin@ID_strumento==22) {
				parseFXForwardName <- function(name) {

					tmp <- strsplit(name," ")[[1]]
					tmp <- remSpaces(tmp)
					names(tmp) <- c("currency","amount","valuta_label","valueDate")
					tmp <- as.list(tmp)
					
					#"CHF -1,000,000.00 Valuta 26-03-2012"
					return(tmp)
				}
				info <- parseFXForwardName(origin@Nome)
				idAAA <- paste(origin@Moneta,info[["valueDate"]],sep="")
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",idAAA),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			
			# consider Unclassified
			if (origin@ID_strumento==31) {
	
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",origin@NumeroValore),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}			
			
			
			string <- "The idFactory method for the following instrument is not implemented:"
			string <- paste(string,
					"\n  Name: ",origin@Nome,
					"\n  ID_strumento: ",origin@ID_strumento,
					"\n  ID_AAA: ",origin@ID_AAA,sep="")
			stop(string)
			
		}



)



