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
			## 51,ETF_commodities_gold
			## 52,Credit_linked_note
			## 53,Unclassified
			## 54,Conto_corrente_fittizio"
			## 55,ETF_commodities_platinum
				
			instrumentsWithISIN <- c(1,2,3,4,5,8:17,23:30,32:38,42:49,51:53,55)
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
			# consider Conto_corrente and conto_corrente_fittizio
			if (is.element(origin@ID_strumento,c(40,54))) {
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",paste(origin@Moneta,tolower(origin@Moneta),sep="-")),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Opzioni_su_azioni
			if (origin@ID_strumento==18) {
				
				info <- parseOptionOnEquityName(origin@Nome)
				idAAA <- paste(info[["ISIN"]],info[["strike"]],info[["callPut"]],info[["maturity"]],sep="")
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",idAAA),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Opzioni_su_divise
			if (origin@ID_strumento==19) {
			#	parseOptionOnFxName <- function(name) {
			#		tmp <- strsplit(name," ")[[1]]
			#		tmp <- str_trim(tmp)
			#		names(tmp) <- c("callPut","maturity","strike_label","strike","underlyingName","amount","premium","premiumCurrency")
			#		tmp <- as.list(tmp)
					
			#		if (tmp[["callPut"]]=="PUT") tmp[["callPut"]] <- "P" else tmp[["callPut"]] <- "C"
			#		tmp[["maturity"]] <- format(strptime(tmp[["maturity"]],format="%d-%m-%y"),"%d-%m-%Y")
			#		#"PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"
			#		return(tmp)
			#	}
			#	info <- parseOptionOnFxName(origin@Nome)
				info <- getOptionParameters(origin)
				
				idAAA <- paste(info[["optionType"]],
							paste(info[["underlying"]],info[["numeraire"]],sep=""),
							info[["expiryDate"]],
							info[["strike"]],
							sep=""
						)
				idAyrton <- new("IdAyrton",
						idAAA=new("IdAAA_character",idAAA),
						idStrumento=origin@ID_strumento)
				
				return(idAyrton)
			}
			# consider Opzioni_su_obbligazioni
			if (origin@ID_strumento==20) {
				parseOptionOnBondName <- function(name) {
					tmp <- strsplit(name," ")[[1]]
					tmp <- str_trim(tmp)
					names(tmp) <- c("callPut","maturity","strike_label","strike","currency","amount","premium","premiumCurrency","ISIN")
					tmp <- as.list(tmp)
					
					if (tmp[["callPut"]]=="PUT") tmp[["callPut"]] <- "P" else tmp[["callPut"]] <- "C"
					tmp[["maturity"]] <- format(strptime(tmp[["maturity"]],format="%d-%m-%y"),"%d-%m-%Y")
					#"PUT 17-08-12 Strike 103.5 EUR 125000 Premio(-345.45 EUR) EU0011027469"
					return(tmp)
				}
				info <- parseOptionOnBondName(origin@Nome)
				idAAA <- paste(info[["ISIN"]],info[["strike"]],info[["callPut"]],info[["maturity"]],sep="")
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
					tmp1 <- str_trim(tmp1)
					partToBeParsed <- tmp1[[1]]
					maturity <- substr(partToBeParsed,nchar(partToBeParsed)-9,nchar(partToBeParsed))
					maturity <- format(strptime(maturity,format="%d-%m-%Y"),"%Y-%m-%d")
					futureName <- substr(partToBeParsed,1,nchar(partToBeParsed)-11)
					futureName <- str_trim(futureName)
					
					#"SMI Futures 16-03-2012 / 10              "
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
					tmp <- str_trim(tmp)
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
			
			string <- "The idFactory method for the following instrument is not implemented:"
			string <- paste(string,
						"\n  Name: ",origin@Nome,
						"\n  ID_strumento: ",origin@ID_strumento,
						"\n  ID_AAA: ",origin@ID_AAA,sep="")
			stop(string)

		}
)

		

