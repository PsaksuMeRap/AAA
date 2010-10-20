optimization.markowitz <- function(client,referenceCurrency,varConfidenceLevel,odbcCon)
{
#options(show.error.messages = FALSE)
#options(warn=1)

# the following abbreviations conventions in name definitions will be used
# RF = risk free
# RC = Reference Currency
# IRC = In Reference Currency
# NIRC = Not In Reference Currency

RC = referenceCurrency
 
cancel <- function () 
  {
    error <- geterrmessage()
    myMessage <- paste("The following error is occurred in the procedure optimization.markowitz:\n",
                       error,sep="")
    tkmessageBox(message=myMessage,icon="error",type="ok")
    return()
  }

options(error=cancel)

# create the odbc Channel if odbcCon is missing
if (missing(odbcCon)) missingOdbcChannel <- T else missingOdbcChannel <- F
if (missingOdbcChannel) odbcCon <- setOdbcConnection("DBMarkowitz")

# Tabella codice messaggi per tabella SQL Messaggi_da_R
# codice 1: I vincoli lineari non sono soddisfabili. Il problema non ammette soluzione.
# codice 2: La matrice di varianza covarianza è singolare.
nbMessage <- 1
# Elimina i dati nella tabella Messaggi_da_R per il client desiderato
query.result <- sqlQuery(odbcCon, paste("DELETE FROM Messaggi_da_R WHERE Cliente LIKE '", client, "'",sep=""))
    
varConfidenceLevel <- as.numeric(varConfidenceLevel)

###-----COSTRUZIONE MATRICE A E VETTORE b PER LA DEFINIZIONE DEI VINCOLI-----###

# Le informazioni riguardanti la struttura delle restrizioni settoriali sono contenute nella
# tabella D_Restrizioni_markowitz_passo_1. Un segno negativo del campo Numero_vincolo
# indica che la restrizione è esatta ed un segno positivo che si tratta di una restrizione di tipo "<=", ">=".

# compute the number of exact constraints (=)
query.result <- sqlQuery(odbcCon, paste("SELECT DISTINCT Numero_vincolo FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo < 0 AND Cliente LIKE '",client,"'",sep = ""))
nbExactConstraints <- nrow(query.result)

# compute the number of inexact constraints (<=, >=)
query.result <- sqlQuery(odbcCon, paste("SELECT DISTINCT Numero_vincolo FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo > 0 AND Cliente LIKE '",client,"'",sep = ""))
nbInexactConstraints <- nrow(query.result)

nbConstraints <- nbExactConstraints + nbInexactConstraints


# compute the number of interest rates not in reference currency
query.result <- sqlQuery(odbcCon,paste("SELECT Moneta FROM E_expected_returns_rf_per_cliente WHERE Cliente LIKE '",client,"' AND Moneta NOT LIKE '",RC,"' ORDER BY Moneta",sep=""))
nb.RF.NIRC <- nrow(query.result)
if (nb.RF.NIRC >0) {
   tickers.RF.NIRC <- as.vector(query.result$Moneta)
}


# compute the number of risk factors
if (nbExactConstraints > 0) {
   query.result <- sqlQuery(odbcCon, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = -1 AND Cliente LIKE '",client,"'",sep = ""))
   nbRiskFactors <- nrow(query.result)   
} else {
   if (nbInexactConstraints > 0) {
      query.result <- sqlQuery(odbcCon, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = 1 AND Cliente LIKE '",client,"'",sep = ""))
      nbRiskFactors <- nrow(query.result)     
   } else {
     # Non ci sono restrizioni né esatte né non esatte. Esci dalla procedura
     print ("Non ci sono restrizioni, fine della procedura.")
     stop(NULL)
   }
}

if (nbExactConstraints > 0) {
   query.result <- sqlQuery(odbcCon, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = -1 AND Cliente LIKE '",client,"' ORDER BY Moneta, Branche",sep = ""))
   nbRiskFactors.new <- nrow(query.result)

   #crea la matrice A ed il vettore b
   A <- matrix(data = query.result$Valore, nrow = 1, ncol = nbRiskFactors, byrow = TRUE)
   b <- as.vector(query.result$b [1])

   i <- -2

   while (i >= -nbExactConstraints) {
      query.result <- sqlQuery(odbcCon, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = ", i, " AND Cliente LIKE '",client,"' ORDER BY Moneta, Branche",sep = ""))
      nbRiskFactors.new <- nrow(query.result)
      # Verifica che il numero di fattori di rischio sia costante
      if (nbRiskFactors != nbRiskFactors.new) {
         print ("Errore di implementazione: il numero di fattori di rischio non è costante!")
         print (paste("Il numero di fattori di rischio è:", nbRiskFactors))
         print (paste("Il numero di fattori di rischio per la restrizione esatta numero",abs(i),"è pari a",nbRiskFactors.new))
         stop(NULL)
      } else {
         # Non e' necessario creare la matrice A ed il vettore b
         A.tmp <- matrix(data = query.result$Valore, nrow = 1, ncol = nbRiskFactors, byrow = TRUE)
         b.tmp <- as.vector(query.result$b [1])
         A <- rbind(A,A.tmp)
         b <- rbind(b,b.tmp)
      }
      i <- i - 1
      rm(A.tmp,b.tmp)
   }  # Fine ciclo sulle restrizioni esatte
   rm (i)
}


# Ora considera le restrizioni non esatte, cioe' quelle maggiore uguale o minore uguale
if (nbInexactConstraints > 0) {
   query.result <- sqlQuery(odbcCon, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = 1 AND Cliente LIKE '",client,"' ORDER BY Moneta, Branche",sep = ""))
   nbRiskFactors.new <- nrow(query.result)
   if (nbExactConstraints == 0) { #in this case we have to create matrix A and vector b
      # Crea la matrice A ed il vettore b
      A <- matrix(data = query.result$Valore, nrow = 1, ncol = nbRiskFactors, byrow = TRUE)
      b <- as.vector(query.result$b [1])
   } else {
      # Verifica che il numero di fattori di rischio sia costante
      if (nbRiskFactors != nbRiskFactors.new) {
         print ("Errore di implementazione: il numero di fattori di rischio non è costante!")
         print (paste("Il numero di fattori di rischio è:", nbRiskFactors))
         print (paste("Il numero di fattori di rischio per la restrizione non esatta numero", i,"è pari a",nbRiskFactors.new))
         stop(NULL)
      } else {
         # Non e' necessario creare la matrice A ed il vettore b in quanto già creato precedentemente
         A.tmp <- matrix(data = query.result$Valore, nrow = 1, ncol = nbRiskFactors, byrow = TRUE)
         b.tmp <- as.vector(query.result$b [1])
         A <- rbind(A,A.tmp)
         b <- rbind(b,b.tmp)
      }
   }

   i <- 2

   while (i <= nbInexactConstraints) {
      query.result <- sqlQuery(odbcCon, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = ", i, " AND Cliente LIKE '",client,"' ORDER BY Moneta, Branche",sep = ""))
      nbRiskFactors.new <- nrow(query.result)
      # Verifica che il numero di fattori di rischio sia costante
      if (nbRiskFactors != nbRiskFactors.new) {
         print ("Errore di implementazione: il numero di fattori di rischio non è costante!")
         print (paste("Il numero di fattori di rischio è:", nbRiskFactors))
         print (paste("Il numero di fattori di rischio per la restrizione non esatta numero", i,"è pari a",nbRiskFactors.new))
         stop(NULL)
      } else {
         # Non e' necessario creare la matrice A ed il vettore b
         A.tmp <- matrix(data = query.result$Valore, nrow = 1, ncol = nbRiskFactors, byrow = TRUE)
         b.tmp <- as.vector(query.result$b [1])
         A <- rbind(A,A.tmp)
         b <- rbind(b,b.tmp)
      }
      i <- i + 1
      rm (A.tmp,b.tmp)
   }  # Fine ciclo sulle restrizioni non esatte
   rm (i)
}


###-----COSTRUZIONE MATRICE DI VARIANZA COVARIANZA-----###


# Estrai la lista delle Monete e dei Settori che insieme servono come Tickers
rm (query.result)
query.result <- sqlQuery(odbcCon, paste("SELECT DISTINCT Moneta AS Currency, Branche FROM D_Restrizioni_markowitz_passo_1 WHERE Cliente LIKE '",client,"' ORDER BY Moneta, Branche",sep = ""))

currencyBranche <- as.matrix(query.result)
riskFactorsId <- paste(currencyBranche[,"Currency"],currencyBranche[,"Branche"],sep = "_")
rm(currencyBranche)

if (nbRiskFactors != length(riskFactorsId)) {
   print ("Serious error in the covariance matrix construction: ")
   print ("The number of risk factors is different from the number of risk factors in the constraint's definition.")
   print ("Stop at 'COSTRUZIONE MATRICE DI VARIANZA COVARIANZA'")
   stop(NULL)
}

S <- matrix(data = NA, nrow =nbRiskFactors , ncol = nbRiskFactors)
dimnames (S) <- list(riskFactorsId,riskFactorsId)

dimnames (A) <- list(NULL,riskFactorsId)

# Estrai la lista delle varianze-covarianze
odbcQuery(odbcCon, "SELECT Ticker1, Ticker2, Covarianza FROM E_covarianze_settoriali")
rm(query.result)
query.result <- sqlGetResults(odbcCon)

lista.tickers <- as.matrix(query.result[,1:2])
covarianze <- as.vector(query.result[,3])

for (i in 1:length(covarianze)) {
   S[lista.tickers[i,1],lista.tickers[i,2]]=covarianze[i]
}


# Annualizza la matrice delle varianze/covarianze
S <- 252*S

if (sum(is.na(S))) {
   print ("La matrice di varianza-covarianza non è completa: errore nella procedura.")
   print ("Verifica la tabella dei fattori di rischio e della matrice di varianza-covarianza")
   stop(NULL)
}

# Calcola gli autovalori e se ce ne sono di <= 0 applica la trasformazione di quelli negativi

a <- eigen(S)
nb.neg.val <- sum(a$values<=0)
if (nb.neg.val > 0) {
   messaggio <- paste("La matrice di covarianza non è positiva definita. Ci sono ",nb.neg.val," autovalori <= 0.",sep="")
   query.result <- sqlQuery(odbcCon,paste("INSERT INTO Messaggi_da_R Values ('",client,"',",nbMessage,",3,'",messaggio,"')", sep=""))
   nbMessage <- nbMessage + 1
   # Calcola la posizione dell'ultimo autovalore positivo
   posizione.ultimo.autovalore <- length(a$values) - nb.neg.val
   ultimo.autovalore <- a$values[posizione.ultimo.autovalore]
   decremento <- -ultimo.autovalore / (nb.neg.val+1)
   for (i in 1:nb.neg.val) {
      vecchio.autovalore <- a$values[posizione.ultimo.autovalore+i]
      a$values[posizione.ultimo.autovalore+i] = ultimo.autovalore + i*decremento
      messaggio <- paste("L`autovalore ", vecchio.autovalore," è stato sostituito con ",a$values[posizione.ultimo.autovalore+i],".",sep="")
      query.result <- sqlQuery(odbcCon,paste("INSERT INTO Messaggi_da_R Values ('",client,"',",nbMessage,",3,'",messaggio,"')", sep=""))
      nbMessage <- nbMessage + 1
   }
    
   SS <- a$vectors %*% diag(a$values) %*% t(a$vectors)
   delta.max = max(SS-S)
   messaggio <- paste("La variazione massima nella matrice di covarianza è di ", delta.max,".",sep="")
   query.result <- sqlQuery(odbcCon,paste("INSERT INTO Messaggi_da_R Values ('",client,"',",nbMessage,",3,'",messaggio,"')", sep=""))
   nbMessage <- nbMessage + 1
   S <- SS
   rm (SS, delta.max, nb.neg.val, posizione.ultimo.autovalore, ultimo.autovalore, decremento)
}


rm (a, lista.tickers, covarianze)


library(quadprog)


# Seleziona l'expected return del risk free interest rate della valuta di riferimento
rm(query.result)
query.result <- sqlQuery(odbcCon, paste("SELECT Moneta, Expectation FROM E_expected_returns_rf_per_cliente WHERE Cliente LIKE '", client, "' AND Moneta LIKE '", RC,"'", sep=""))

i <- nrow(query.result)

if (i>0) {
   isRequired.RF.RC <- TRUE
   return.RF.IRC <- query.result[1,2]
} else {
   isRequired.RF.RC <- FALSE
}

rm(i)


# Costruisci i weights della restrizione esatta che determina il livello desiderato di rendita del portafoglio
rm("query.result")
query.result <- sqlQuery(odbcCon, paste("SELECT MonetaBranche, Expectation FROM E_expected_returns_per_cliente_finali WHERE Cliente LIKE '", client, "' ORDER BY MonetaBranche", sep=""))

valori <- as.vector(query.result[,2])
names(valori) <- as.vector(query.result[,1])

# Inserisci i dati nel giusto ordine
A.tmp <- matrix(valori[riskFactorsId], nrow = 1, ncol = nbRiskFactors, byrow = TRUE)

rm(valori)

i <- sum(is.na(A.tmp))
if (i > 0) {
   print("Errore nella fase finale: la restrizione principale sul return del portafoglio contiene NA quali valori.")
   interrompi <- TRUE
} else {
   interrompi <- FALSE
}

rm(i)

if (interrompi) {
   print("La procedura è interrotta")
} else {
   nbExactConstraints  <- nbExactConstraints + 1
   nbConstraints <- nbExactConstraints + nbInexactConstraints
   A <- rbind(A.tmp,A)
   b <- rbind(0,b)

}

rm(A.tmp,interrompi)

# Cacolo del range per il calcolo della curva
# 1) determina in base al valore del risk free in moneta di riferimento i
#    criteri per il calcolo del range
continua <- TRUE
query.result <- sqlQuery(odbcCon, paste("SELECT MonetaBranche, Expectation_mon_rif FROM E_exp_excess_ret_in_mon_rif_totali WHERE Cliente LIKE '", client, "' ORDER BY MonetaBranche", sep=""))
valori <- as.vector(query.result[,2])

if (!isRequired.RF.RC) {    #il risk free non è voluto in portafoglio
   minimo <- max(c(0,min(valori)))
   massimo <- max(valori)
   if (massimo <= minimo) {
      print ("Errore nella fase di preparazione. Il massimo expected return è minore o ugale al minimo expected return.")
      continua <- FALSE
   }
} else {
   minimo <- 0
   massimo <- max(valori)
   if (massimo <= minimo) {
      print ("Errore nella fase di preparazione. Almeno un expected return deve essere maggiore del risk free in moneta di riferimento")
      continua <- FALSE
   }
}



if (continua) {
   # identify the indexes of some classes of risk factors 
   
   #1) which risk factors are equities with currency equal to the RC
   index.equities.IRC <- grep(RC,riskFactorsId)
   if (length(index.equities.IRC)>0) {
      tmp <- rep(FALSE,nbRiskFactors)
      tmp[index.equities.IRC] <- TRUE
      index.equities.IRC <- tmp
      rm(tmp)
   }

   #2) which risk factors are currencies
   index.fx <- grep("_Moneta",riskFactorsId)
   if (length(index.fx)>0) {
      tmp <- rep(FALSE,nbRiskFactors)
      tmp[index.fx] <- TRUE
      index.fx <- tmp
      rm(tmp)
   }
 
   #3) which risk factors are equities
   if (length(index.fx) == 0){
      index.equities <- rep(TRUE,nbRiskFactors)
   } else {
      index.equities <- !index.fx
   }

   #4) Determina i risk factors nelle diverse valute (senza quella di riferimento)
   #   le matrici scomposizione.monete.* serviranno per calcolare i weights dei risk free
   #   nelle varie monete
   if (length(index.fx)>0) {
      elenco.monete <- substr(riskFactorsId[index.fx],1,3)
      tmp1 <- substr(riskFactorsId, 1, 3)
      tmp2 <- substr(riskFactorsId,5,nchar(riskFactorsId))

      for (moneta in elenco.monete) {
         true1 <- (tmp1 == moneta) & (tmp2 == "Moneta")
         true2 <- (tmp1 == moneta) & (!true1)
         if (exists("index.currencies.fx")) {
            index.currencies.fx <- rbind(index.currencies.fx,true1)
            index.branche.per.monete <- rbind(index.branche.per.monete,true2)
         } else {
            index.currencies.fx <- matrix(true1,nrow=1)
            index.branche.per.monete <- matrix(true2,nrow=1)
         }
         rm(true1,true2)
      }

      rm (moneta)

      dimnames(index.currencies.fx) <- list(elenco.monete, substr(riskFactorsId,1,8))
      dimnames(index.branche.per.monete) <- list(elenco.monete, substr(riskFactorsId,1,8))
      rm (tmp1,tmp2)
   }

   ##5) Determina il numero di branche
   nb.branche <- sum(index.equities)
   nb.fx <- nbRiskFactors - nb.branche


   ##6) determina quali sono le valute per cui c'e' un risk free
   query.result <- sqlQuery(odbcCon, paste("SELECT Moneta FROM A_risk_free_selezionati WHERE Cliente LIKE '", client, "' AND Moneta NOT LIKE '",RC,"' ORDER BY Moneta", sep=""))
   i <- nrow(query.result)
   if (i > 0) {
      moneta.RF.NIRC <- as.vector(query.result[,])
      names(moneta.RF.NIRC) <- moneta.RF.NIRC
   }

   if (minimo != massimo) nbOptimizations = 20 else nbOptimimizations = 1
  
   for (desired.RF in seq(from=minimo,to=massimo,length.out=nbOptimizations)) 
    {
      b[1] <- desired.RF
      solution <- solve.QP(S, rep(0,nbRiskFactors), t(A), b, meq=nbExactConstraints)
                         
      if (!is.list(solution)) {
         if (solution == "Errore 1") {
            messaggio <- paste("I vincoli lineari non sono soddisfabili. Il problema non ammette soluzione per rendimenti uguali a ",desired.RF,".",sep="")
            query.result <- sqlQuery(odbcCon,paste("INSERT INTO Messaggi_da_R Values ('",client,"',",nbMessage,",1,'",messaggio,"')", sep=""))
            nbMessage <- nbMessage + 1
            next
         } else {
            messaggio <- "La matrice di varianza covarianza è singolare."
            query.result <- sqlQuery(odbcCon,paste("INSERT INTO Messaggi_da_R Values ('",client,"',",nbMessage,",2,'",messaggio,"')", sep=""))
            nbMessage <- nbMessage + 1
            break
         }
      }

      ## save the expected return of the portfolio: if the risk free in reference currency is required 
      ## then the expected return of the portfolio is equal to "risk free in reference currency + desired.RF"
      if (!isRequired.RF.RC) {
         mu.tmp <- desired.RF
      } else {
         mu.tmp <- desired.RF + return.RF.IRC
      }
      if (exists("mu")) {
         mu <- c(mu, mu.tmp)
      } else {
        mu <- mu.tmp
      }

      ## save the optimal volatility
      sigma.tmp <- sqrt(2.0*solution$value)
      if (exists("sigma")) {
         sigma <- c(sigma, sigma.tmp)
      } else {
         sigma <- sigma.tmp
      }

      ## save the corresponding VaR
      if (exists("VaRisk")) {
        VaRisk <- c(VaRisk,qnorm(varConfidenceLevel, mean=mu.tmp, sd=sigma.tmp))         
      } else {
        VaRisk <- qnorm(varConfidenceLevel, mean=mu.tmp, sd=sigma.tmp)
      }

      ## save all the optimisation's output
      if (exists("lista.soluzioni")) {
         lista.soluzioni[[length(lista.soluzioni)+1]] <- solution
      } else {
         lista.soluzioni <- list(solution)
      }

      # save the weights of all risk factors 
      if (exists("weightsRiskFactors")) {
         weightsRiskFactors <- rbind(weightsRiskFactors,solution$solution)
      } else {
         weightsRiskFactors <- matrix(solution$solution, nrow=1)
         dimnames(weightsRiskFactors) <- list(NULL,riskFactorsId)
      }

      ## save the weights of the risk free (if required)
      if (isRequired.RF.RC) {
         if (exists("weights.RF")) {
            somma <- 1 - sum(solution$solution[index.equities.IRC])
            somma <- somma - sum(solution$solution[index.fx])
            weights.RF <- c(weights.RF,somma)
         } else {
            somma <- 1 - sum(solution$solution[index.equities.IRC])
            somma <- somma - sum(solution$solution[index.fx])
            weights.RF <- somma
         }
      }

      ## compute the weights for every risk free not in reference currency
      ## a) compute the sum over the branche
      if (length(index.fx)>0) {
         if (exists("sum.weights.branche.NIRC")) {
            sum.weights.branche.NIRC <- cbind(sum.weights.branche.NIRC,rep(0,sum(index.fx)))
            weights.fx.NIRC <- cbind(weights.fx.NIRC,rep(0,sum(index.fx)))
         } else {
            sum.weights.branche.NIRC <- matrix(nrow=sum(index.fx),ncol=1)
            dimnames(sum.weights.branche.NIRC) <- list(elenco.monete,NULL)
            weights.fx.NIRC <- matrix(nrow=sum(index.fx),ncol=1)
            dimnames(weights.fx.NIRC) <- list(elenco.monete,NULL)
         }
         i <- dim(sum.weights.branche.NIRC)[2]
         for (moneta in elenco.monete) {
            sum.weights.branche.NIRC[moneta,i] <- sum(solution$solution[index.branche.per.monete[moneta,]])
            weights.fx.NIRC[moneta,i] <- sum(solution$solution[index.currencies.fx[moneta,]])
         }
      }

      ## compute the weights for the equities
      if (nb.branche > 0 ) {
         if (exists("weights.equities")) {
            weights.equities <- rbind(weights.equities,solution$solution[index.equities])
         } else {
            weights.equities <- matrix(solution$solution[index.equities], nrow=1)
            dimnames(weights.equities) <- list(NULL,riskFactorsId[index.equities])
         }
      }

      ## Salva il vettore dei weights degli fx in una matrice
      if (nb.fx > 0) {
         if (exists("weights.fx")) {
            weights.fx <- rbind(weights.fx,solution$solution[index.fx])
         } else {
            weights.fx <- matrix(solution$solution[index.fx], nrow=1)
            dimnames(weights.fx) <- list(NULL,riskFactorsId[index.fx])
         }
      }
      rm(solution)
   } ## end of "for (desired.RF in seq(from=minimo,to=massimo,length.out=nbOptimizations))"


   # Determina il minimo rispetto alla varianza nella curva media-varianza
   sigma.minimo <- min(sigma)
   index.valori.efficienti <- mu >= mu[sigma==sigma.minimo]

   results <- list(mu = mu[index.valori.efficienti],sigma=sigma[index.valori.efficienti],
                  VaRisk=VaRisk[index.valori.efficienti],soluzioni=lista.soluzioni[index.valori.efficienti],
                  weightsRiskFactors=weightsRiskFactors[index.valori.efficienti,])
  
   if (exists("weights.RF")) {
      results[["weights.RF"]] = weights.RF[index.valori.efficienti]
      portfolio <- matrix(results[["weights.RF"]],ncol=1)
      colnames(portfolio) <- paste(referenceCurrency,"_Risk free",sep="")
   }

   if (length(index.fx)>0) {
      weights.RF.NIRC <- weights.fx.NIRC - sum.weights.branche.NIRC
      tmp <- dimnames(weights.fx.NIRC)[[1]]
      results[["weights.RF.NIRC"]] = weights.RF.NIRC[,index.valori.efficienti]
      rownames(results[["weights.RF.NIRC"]]) = paste(tmp,"_Risk free",sep="")

      if (exists("portfolio"))
        {
          portfolio <- cbind(portfolio,t(results[["weights.RF.NIRC"]]))
        }
      else
        {
          portfolio <- matrix(t(results[["weights.RF.NIRC"]]),ncol=1)
        }  
   }
   
   if (exists("weights.equities")) {
      results[["weights.equities"]] = weights.equities[index.valori.efficienti,]
      if (exists("portfolio"))
        {
          portfolio <- cbind(portfolio,results[["weights.equities"]])
        }
      else
        {
          portfolio <- matrix(results[["weights.equities"]],ncol=1)
        }       
   }
   

   tmp <- dimnames(portfolio)[[2]]
   tmp <- unlist(strsplit(tmp, "_"))
   index1 <- seq(start=1,to=length(tmp),by=2)
   index2 <- (1:length(tmp))[-index1]
   tmpCurrency <- matrix(tmp[index1],nrow=1)
   tmpSector <- matrix(tmp[index2],nrow=1)
   portfolio <- rbind(tmpCurrency,tmpSector,portfolio)
   rm(tmp,tmpCurrency,tmpSector)

   results[["portfolio"]] <- portfolio
   
   # no more needed 
   #if (exists("weights.equities")){
   #   # results[[length(results)+1]] = weights.equities[index.valori.efficienti]
   #   results[["weightsEquities"]] = weights.equities[index.valori.efficienti]
   #}
   # no more needed
   #if (exists("weights.fx")) {
   #   # results[[length(results)+1]] = weights.fx[index.valori.efficienti]
   #   results[["weightsFx"]] = weights.fx[index.valori.efficienti]
   #}

} # fine if (continua)

rm (query.result)

# Rimuovi i results precedenti
stringa = paste("DELETE FROM R_risultati_ottimizzazione_per_cliente WHERE Cliente LIKE '", client,"'",sep="")
query.result <- sqlQuery(odbcCon, stringa)
rm (query.result)

for (i in 1:length(mu)) {
   for (j in 1:nbRiskFactors) {
      if (length(grep("_Moneta",riskFactorsId[j])) == 0) {
         stringa = "INSERT INTO R_risultati_ottimizzazione_per_Cliente (Cliente, Mu, Sigma, VaRisk, Ticker, Peso) VALUES ("
         stringa = paste(stringa,"'",client,"'",sep="")
         stringa = paste(stringa,mu[i],sigma[i],VaRisk[i],sep=",")
         stringa = paste(stringa,",","'",riskFactorsId[j],"'",sep="")
         stringa = paste(stringa,weightsRiskFactors[i,j],sep=",")
         stringa = paste(stringa,")",sep="")
         query.result <- sqlQuery(odbcCon, stringa)
         rm (query.result)
      } else {
         if (exists("moneta.RF.NIRC")) {  #ci sono risk free e per cui occorre fare un loop su tutte le monete <> moneta rif.
            moneta <- substr(riskFactorsId[j],1,3)
            for (mon in moneta.RF.NIRC) {
               if (mon==moneta) { #esiste il RF nella moneta desiderata
                  stringa = "INSERT INTO R_risultati_ottimizzazione_per_clinte (Client, Mu, Sigma, VaRisk, Ticker, Peso) VALUES ("
                  stringa = paste(stringa,"'",client,"'",sep="")
                  stringa = paste(stringa,mu[i],sigma[i],VaRisk[i],sep=",")
                  stringa = paste(stringa,",","'",moneta,"_Moneta","'",sep="")
                  stringa = paste(stringa,weights.RF.NIRC[moneta,i],sep=",")
                  stringa = paste(stringa,")",sep="")
                  query.result <- sqlQuery(odbcCon, stringa)
                  rm (query.result)
                  break
               }
            }
         }
      }
   }

   # Se il risk free in moneta di riferimento è desiderato allora introduci anche il peso per il risk free
   if (isRequired.RF.RC) {
      stringa = "INSERT INTO R_risultati_ottimizzazione_per_cliente (Cliente, Mu, Sigma, VaRisk, Ticker, Peso) VALUES ("
      stringa = paste(stringa,"'",client,"'",sep="")
      stringa = paste(stringa,mu[i],sigma[i],VaRisk[i],sep=",")
      stringa = paste(stringa,",","'",RC,"_Moneta","'",sep="")
      stringa = paste(stringa,weights.RF[i],sep=",")
      stringa = paste(stringa,")",sep="")
      query.result <- sqlQuery(odbcCon, stringa)
      rm (query.result)
   }
}

# close odbcCon
if (missingOdbcChannel) odbcClose(odbcCon)

# Stampa i grafici desiderati
#png(filename = "c:/R/MediaVarianza.png", width = 600, height = 450, pointsize = 12)
#plot(sigma[index.valori.efficienti],mu[index.valori.efficienti],main=paste("Media-Varianza per ",client),type="l",ylab="E[r]",xlab=expression(sigma),lwd=2)
#dev.off()

#png(filename = "c:/R/VARMedia.png", width = 600, height = 450, pointsize = 12)
#plot(-VaRisk[index.valori.efficienti],mu[index.valori.efficienti],main=paste("E[R]-Var per ",client),ylab="Expected Return",xlab="VAR",type="l",lwd=2)
#dev.off()

#save.image(file = paste(client,".RData",sep=""), safe = FALSE)
  options(error=NULL)
  ## add the client, referenceCurrency and varConfidenceLevel to the list
  results[["client"]] <- client
  results[["referenceCurrency"]] <- referenceCurrency
  results[["varConfidenceLevel"]] <- varConfidenceLevel
  
  return(results)
}