test.shouldCreateDefaultPortfolio <- function() {


setGeneric("portfoliosFactory",def=function(positions,...) standardGeneric("portfoliosFactory"))

setMethod("portfoliosFactory",signature(positions="AyrtonPositions"),
		function(positions,politicaInvestimento.df) {
			
			if (missing(politicaInvestimento.df)) {
				# determina la moneta di riferimento del cliente
				query = paste("SELECT A.MonetaInvestimento ",
						"FROM [Sistema (prova)].dbo.DBPoliticaInvestimento A ",
						"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B on A.Cliente = B.Cliente "
						)
				
				politicaInvestimento.df <- sqlQuery(db.prezziStoriciVAR,query,as.is=TRUE)
				
				if (nrow(refCurrency.df)==0) {
					message <- "Impossibile determinare la moneta di investimento. La tabella DBPoliticaInvestimento è vuota."
					message <- paste(message,"Utilizzata moneta di default (CHF) per tutti i portafogli.",sep="\n")
					tkmessageBox(message=message,icon="error",type="ok")
					useDefaultReferenceCurrency <- TRUE
				} else {
					useDefaultReferenceCurrency <- FALSE
				}
				
			}

			owners <- lapply(positions,function(x) return(x@Cliente))
			for (owner in owners) {
				extract <- owners==owner
				usePositions <- positions[extract]
				?? arrivato qui
			}
			
			
			
			isDesiredOwner <- politicaInvestimento.df[,"ID"] == owner
			if (sum(isDesiredOwner)==1) {
				portfolio$refCurrency <- politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]
			} else {
				message <- paste("Impossibile determinare la moneta di investimento di '",owner,"'",sep="")
				message <- paste(message,"Utilizzata moneta di default (CHF).",sep="\n")
				tkmessageBox(message=message,icon="error",type="ok")
			}
		}

)










  # uses a default method
  source("./unitTests/utilities/allocateTestRepositories.R")  
  
  # create the instrument repository  
  allocateTestRepositories("instruments")
  
  # create the origin
  repository <- createRepositoryAyrtonPositions()
  origin <- repository$unclassified1
  
  position <- positionFactory(origin)
  
  checkEquals(position@quantity,100.3)
  checkEquals(position@value,toMoney(123.55,new("Currency","CHF")))
  

}
