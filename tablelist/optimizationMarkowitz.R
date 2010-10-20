optimization.markowitz <- function(cliente,moneta.riferimento,confidenza.var,limite.var)
  {
#options(show.error.messages = FALSE)
#options(warn=1)


termina <- function () {
   errore <- geterrmessage()
   write(errore,"error.txt")
}

options(error=termina)


# Tabella codice messaggi per tabella SQL Messaggi_da_R
# codice 1: I vincoli lineari non sono soddisfabili. Il problema non ammette soluzione.
# codice 2: La matrice di varianza covarianza è singolare.
numero.messaggio <- 1 


###-----COSTRUZIONE MATRICE A E VETTORE b PER LA DEFINIZIONE DEI VINCOLI-----###


# Le informazioni riguardanti la struttura delle restrizioni settoriali sono contenute nella
# tabella D_Restrizioni_markowitz_passo_1. Il segno (positivo o negativo) del campo Numero_vincolo
# indica se la restrizione è esatta (segno negativo) o se è una restrizione del tipo <=, >=.

# Determina il numero di restrizioni esatte (=)
risultato.query <- sqlQuery(channel.markowitz, paste("SELECT DISTINCT Numero_vincolo FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo < 0 AND cliente LIKE '",cliente,"'",sep = ""))
nb.exact.restrictions <- nrow(risultato.query)

# Determina il numero di restrizioni non esatte (<=, >=)
risultato.query <- sqlQuery(channel.markowitz, paste("SELECT DISTINCT Numero_vincolo FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo > 0 AND cliente LIKE '",cliente,"'",sep = ""))
nb.not.exact.restrictions <- nrow(risultato.query)

nb.restrictions <- nb.exact.restrictions + nb.not.exact.restrictions

# Determina il numero di risk free non in moneta di riferimento
risultato.query <- sqlQuery(channel.markowitz,paste("SELECT Moneta FROM E_expected_returns_rf_per_cliente WHERE cliente LIKE '",cliente,"' AND Moneta NOT LIKE '",moneta.riferimento,"' ORDER BY Moneta",sep=""))
nb.rf.not.in.mon.rif <- nrow(risultato.query)
if (nb.rf.not.in.mon.rif >0) {
   tickers.rf.not.in.mon.rif <- as.vector(risultato.query$Moneta)
}


# Determina il numero di fattori di rischio presenti
if (nb.exact.restrictions > 0) {
   risultato.query <- sqlQuery(channel.markowitz, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = -1 AND cliente LIKE '",cliente,"'",sep = ""))
   nb.risk.factors <- nrow(risultato.query)   
} else {
   if (nb.not.exact.restrictions > 0) {
      risultato.query <- sqlQuery(channel.markowitz, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = 1 AND cliente LIKE '",cliente,"'",sep = ""))
      nb.risk.factors <- nrow(risultato.query)     
   } else {
     # Non ci sono restrizioni né esatte né non esatte. Esci dalla procedura
     print ("Non ci sono restrizioni, fine della procedura.")
     stop(NULL)
   }
}

if (nb.exact.restrictions > 0) {
   risultato.query <- sqlQuery(channel.markowitz, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = -1 AND cliente LIKE '",cliente,"' ORDER BY Moneta, Branche",sep = ""))
   nb.risk.factors.new <- nrow(risultato.query)

   #crea la matrice A ed il vettore b
   A <- matrix(data = risultato.query$Valore, nrow = 1, ncol = nb.risk.factors, byrow = TRUE)
   b <- as.vector(risultato.query$b [1])

   i <- -2

   while (i >= -nb.exact.restrictions) {
      risultato.query <- sqlQuery(channel.markowitz, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = ", i, " AND cliente LIKE '",cliente,"' ORDER BY Moneta, Branche",sep = ""))
      nb.risk.factors.new <- nrow(risultato.query)
      # Verifica che il numero di fattori di rischio sia costante
      if (nb.risk.factors != nb.risk.factors.new) {
         print ("Errore di implementazione: il numero di fattori di rischio non è costante!")
         print (paste("Il numero di fattori di rischio è:", nb.risk.factors))
         print (paste("Il numero di fattori di rischio per la restrizione esatta numero",abs(i),"è pari a",nb.risk.factors.new))
         stop(NULL)
      } else {
         # Non e' necessario creare la matrice A ed il vettore b
         A.tmp <- matrix(data = risultato.query$Valore, nrow = 1, ncol = nb.risk.factors, byrow = TRUE)
         b.tmp <- as.vector(risultato.query$b [1])
         A <- rbind(A,A.tmp)
         b <- rbind(b,b.tmp)
      }
      i <- i - 1
      rm(A.tmp,b.tmp)
   }  # Fine ciclo sulle restrizioni esatte
   rm (i)
}


# Ora considera le restrizioni non esatte, cioe' quelle maggiore uguale o minore uguale
if (nb.not.exact.restrictions > 0) {
   risultato.query <- sqlQuery(channel.markowitz, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = 1 AND cliente LIKE '",cliente,"' ORDER BY Moneta, Branche",sep = ""))
   nb.risk.factors.new <- nrow(risultato.query)
   if (nb.exact.restrictions == 0) { #in this case we have to create matrix A and vector b
      # Crea la matrice A ed il vettore b
      A <- matrix(data = risultato.query$Valore, nrow = 1, ncol = nb.risk.factors, byrow = TRUE)
      b <- as.vector(risultato.query$b [1])
   } else {
      # Verifica che il numero di fattori di rischio sia costante
      if (nb.risk.factors != nb.risk.factors.new) {
         print ("Errore di implementazione: il numero di fattori di rischio non è costante!")
         print (paste("Il numero di fattori di rischio è:", nb.risk.factors))
         print (paste("Il numero di fattori di rischio per la restrizione non esatta numero", i,"è pari a",nb.risk.factors.new))
         stop(NULL)
      } else {
         # Non e' necessario creare la matrice A ed il vettore b in quanto già creato precedentemente
         A.tmp <- matrix(data = risultato.query$Valore, nrow = 1, ncol = nb.risk.factors, byrow = TRUE)
         b.tmp <- as.vector(risultato.query$b [1])
         A <- rbind(A,A.tmp)
         b <- rbind(b,b.tmp)
      }
   }

   i <- 2

   while (i <= nb.not.exact.restrictions) {
      risultato.query <- sqlQuery(channel.markowitz, paste("SELECT * FROM D_Restrizioni_markowitz_passo_1 WHERE numero_vincolo = ", i, " AND cliente LIKE '",cliente,"' ORDER BY Moneta, Branche",sep = ""))
      nb.risk.factors.new <- nrow(risultato.query)
      # Verifica che il numero di fattori di rischio sia costante
      if (nb.risk.factors != nb.risk.factors.new) {
         print ("Errore di implementazione: il numero di fattori di rischio non è costante!")
         print (paste("Il numero di fattori di rischio è:", nb.risk.factors))
         print (paste("Il numero di fattori di rischio per la restrizione non esatta numero", i,"è pari a",nb.risk.factors.new))
         stop(NULL)
      } else {
         # Non e' necessario creare la matrice A ed il vettore b
         A.tmp <- matrix(data = risultato.query$Valore, nrow = 1, ncol = nb.risk.factors, byrow = TRUE)
         b.tmp <- as.vector(risultato.query$b [1])
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
rm (risultato.query)
risultato.query <- sqlQuery(channel.markowitz, paste("SELECT DISTINCT Moneta, Branche FROM D_Restrizioni_markowitz_passo_1 WHERE cliente LIKE '",cliente,"' ORDER BY Moneta, Branche",sep = ""))

moneta.settore <- as.matrix(risultato.query)
Ticker <- paste(moneta.settore[,1],moneta.settore[,2],sep = "_")
rm(moneta.settore)

if (nb.risk.factors != length(Ticker)) {
   print ("Errore nella costruzione della matrice di varianza-covarianza: ")
   print ("Il numero di fattori di rischio non corrispondono a quelli inseriti nelle restrizioni.")
   print ("Stop at 'COSTRUZIONE MATRICE DI VARIANZA COVARIANZA'")
   stop(NULL)
}

S <- matrix(data = NA, nrow =nb.risk.factors , ncol = nb.risk.factors)
dimnames (S) <- list(Ticker,Ticker)

dimnames (A) <- list(NULL,Ticker)

# Estrai la lista delle varianze-covarianze
odbcQuery(channel.markowitz, "SELECT Ticker1, Ticker2, Covarianza FROM E_covarianze_settoriali")
rm(risultato.query)
risultato.query <- sqlGetResults(channel.markowitz)

lista.tickers <- as.matrix(risultato.query[,1:2])
covarianze <- as.vector(risultato.query[,3])

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
   risultato.query <- sqlQuery(channel.markowitz,paste("INSERT INTO Messaggi_da_R Values ('",cliente,"',",numero.messaggio,",3,'",messaggio,"')", sep=""))
   numero.messaggio <- numero.messaggio + 1
   # Calcola la posizione dell'ultimo autovalore positivo
   posizione.ultimo.autovalore <- length(a$values) - nb.neg.val
   ultimo.autovalore <- a$values[posizione.ultimo.autovalore]
   decremento <- -ultimo.autovalore / (nb.neg.val+1)
   for (i in 1:nb.neg.val) {
      vecchio.autovalore <- a$values[posizione.ultimo.autovalore+i]
      a$values[posizione.ultimo.autovalore+i] = ultimo.autovalore + i*decremento
      messaggio <- paste("L`autovalore ", vecchio.autovalore," è stato sostituito con ",a$values[posizione.ultimo.autovalore+i],".",sep="")
      risultato.query <- sqlQuery(channel.markowitz,paste("INSERT INTO Messaggi_da_R Values ('",cliente,"',",numero.messaggio,",3,'",messaggio,"')", sep=""))
      numero.messaggio <- numero.messaggio + 1
   }
    
   SS <- a$vectors %*% diag(a$values) %*% t(a$vectors)
   delta.max = max(SS-S)
   messaggio <- paste("La variazione massima nella matrice di covarianza è di ", delta.max,".",sep="")
   risultato.query <- sqlQuery(channel.markowitz,paste("INSERT INTO Messaggi_da_R Values ('",cliente,"',",numero.messaggio,",3,'",messaggio,"')", sep=""))
   numero.messaggio <- numero.messaggio + 1
   S <- SS
   rm (SS, delta.max, nb.neg.val, posizione.ultimo.autovalore, ultimo.autovalore, decremento)
}


rm (a, lista.tickers, covarianze)


library(quadprog)


# Seleziona l'expected return del risk free interest rate della valuta di riferimento
rm(risultato.query)
risultato.query <- sqlQuery(channel.markowitz, paste("SELECT Moneta, Expectation FROM E_expected_returns_rf_per_cliente WHERE cliente LIKE '", cliente, "' AND Moneta LIKE '", moneta.riferimento,"'", sep=""))

i <- nrow(risultato.query)

if (i>0) {
   desiderata.moneta.riferimento <- TRUE
   rf.moneta.riferimento <- risultato.query[1,2]
} else {
   desiderata.moneta.riferimento <- FALSE
}

rm(i)


# Costruisci i pesi della restrizione esatta che determina il livello desiderato di rendita del portafoglio
rm("risultato.query")
risultato.query <- sqlQuery(channel.markowitz, paste("SELECT MonetaBranche, Expectation FROM E_expected_returns_per_cliente_finali WHERE cliente LIKE '", cliente, "' ORDER BY MonetaBranche", sep=""))

valori <- as.vector(risultato.query[,2])
names(valori) <- as.vector(risultato.query[,1])

# Inserisci i dati nel giusto ordine
A.tmp <- matrix(valori[Ticker], nrow = 1, ncol = nb.risk.factors, byrow = TRUE)

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
   nb.exact.restrictions  <- nb.exact.restrictions + 1
   nb.restrictions <- nb.exact.restrictions + nb.not.exact.restrictions
   A <- rbind(A.tmp,A)
   b <- rbind(0,b)

}

rm(A.tmp,interrompi)

# Cacolo del range per il calcolo della curva
# 1) determina in base al valore del risk free in moneta di riferimento i
#    criteri per il calcolo del range
continua <- TRUE
risultato.query <- sqlQuery(channel.markowitz, paste("SELECT MonetaBranche, Expectation_mon_rif FROM E_exp_excess_ret_in_mon_rif_totali WHERE cliente LIKE '", cliente, "' ORDER BY MonetaBranche", sep=""))
valori <- as.vector(risultato.query[,2])

if (!desiderata.moneta.riferimento) {    #il risk free non è voluto in portafoglio
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
   # Determina gli indici di alcune classi di Tickers

   #1) Determina quali risk factors sono azioni in moneta di riferimento
   indice.az.in.mon.rif <- grep(moneta.riferimento,Ticker)
   if (length(indice.az.in.mon.rif)>0) {
      tmp <- rep(FALSE,nb.risk.factors)
      tmp[indice.az.in.mon.rif] <- TRUE
      indice.az.in.mon.rif <- tmp
      rm(tmp)
   }

   #2) Determina quali risk factors sono monete
   indice.fx <- grep("_Moneta",Ticker)
   if (length(indice.fx)>0) {
      tmp <- rep(FALSE,nb.risk.factors)
      tmp[indice.fx] <- TRUE
      indice.fx <- tmp
      rm(tmp)
   }
 
   #3) Determina quali risk factors sono azioni
   if (length(indice.fx) == 0){
      indice.az <- rep(TRUE,nb.risk.factors)
   } else {
      indice.az <- !indice.fx
   }

   #4) Determina i risk factors nelle diverse valute (senza quella di riferimento)
   #   le matrici scomposizione.monete.* serviranno per calcolare i pesi dei risk free
   #   nelle varie monete
   if (length(indice.fx)>0) {
      elenco.monete <- substr(Ticker[indice.fx],1,3)
      tmp1 <- substr(Ticker, 1, 3)
      tmp2 <- substr(Ticker,5,nchar(Ticker))

      for (moneta in elenco.monete) {
         true1 <- (tmp1 == moneta) & (tmp2 == "Moneta")
         true2 <- (tmp1 == moneta) & (!true1)
         if (exists("indice.monete.fx")) {
            indice.monete.fx <- rbind(indice.monete.fx,true1)
            indice.branche.per.monete <- rbind(indice.branche.per.monete,true2)
         } else {
            indice.monete.fx <- matrix(true1,nrow=1)
            indice.branche.per.monete <- matrix(true2,nrow=1)
         }
         rm(true1,true2)
      }

      rm (moneta)

      dimnames(indice.monete.fx) <- list(elenco.monete, substr(Ticker,1,8))
      dimnames(indice.branche.per.monete) <- list(elenco.monete, substr(Ticker,1,8))
      rm (tmp1,tmp2)
   }

   #5) Determina il numero di branche
   nb.branche <- sum(indice.az)
   nb.fx <- nb.risk.factors - nb.branche


   #6) determina quali sono le valute per cui c'e' un risk free
   risultato.query <- sqlQuery(channel.markowitz, paste("SELECT Moneta FROM A_risk_free_selezionati WHERE cliente LIKE '", cliente, "' AND Moneta NOT LIKE '",moneta.riferimento,"' ORDER BY Moneta", sep=""))
   i <- nrow(risultato.query)
   if (i > 0) {
      moneta.rf.non.mon.rif <- as.vector(risultato.query[,])
      names(moneta.rf.non.mon.rif) <- moneta.rf.non.mon.rif
   }
   #7) Elimina i dati nella tabella Messaggi_da_R per il cliente desiderato
   rm(risultato.query)
   risultato.query <- sqlQuery(channel.markowitz, paste("DELETE FROM Messaggi_da_R WHERE cliente LIKE '", cliente, "'",sep=""))

   for (r.desiderato in seq(minimo,massimo,0.001)) {
      b[1] <- r.desiderato
      solution <- solve.QP(S, rep(0,nb.risk.factors), t(A), b, meq=nb.exact.restrictions)

      if (!is.list(solution)) {
         if (solution == "Errore 1") {
            messaggio <- paste("I vincoli lineari non sono soddisfabili. Il problema non ammette soluzione per rendimenti uguali a ",r.desiderato,".",sep="")
            risultato.query <- sqlQuery(channel.markowitz,paste("INSERT INTO Messaggi_da_R Values ('",cliente,"',",numero.messaggio,",1,'",messaggio,"')", sep=""))
            numero.messaggio <- numero.messaggio + 1
            next
         } else {
            messaggio <- "La matrice di varianza covarianza è singolare."
            risultato.query <- sqlQuery(channel.markowitz,paste("INSERT INTO Messaggi_da_R Values ('",cliente,"',",numero.messaggio,",2,'",messaggio,"')", sep=""))
            numero.messaggio <- numero.messaggio + 1
            break
         }
      }

      # Salva il giusto expected return del portafoglio (se il risk free è
      # presente, occorre aggiungerlo ad r.desiderato)
      if (!desiderata.moneta.riferimento) {
         mu.tmp <- r.desiderato
      } else {
         mu.tmp <- r.desiderato + rf.moneta.riferimento
      }

      if (exists("mu")) {
         mu <- c(mu, mu.tmp )
      } else {
        mu <- mu.tmp
      }

      # Salva la volatilità
      sigma.tmp <- sqrt(2.0*solution$value)
      if (exists("sigma")) {
         sigma <- c(sigma, sigma.tmp)
      } else {
         sigma <- sigma.tmp
      }

      # Salva il VAR associato
      if (exists("VaRisk")) {
        VaRisk <- c(VaRisk,qnorm(confidenza.var, mean=mu.tmp, sd=sigma.tmp))         
      } else {
        VaRisk <- qnorm(confidenza.var, mean=mu.tmp, sd=sigma.tmp)
      }

      # Salva tutto l'output dell'ottimizzazione
      if (exists("lista.soluzioni")) {
         lista.soluzioni[[length(lista.soluzioni)+1]] <- solution
      } else {
         lista.soluzioni <- list(solution)
      }

      # Salva solo i pesi di tutti i risk factors 
      if (exists("pesi.risk.factors")) {
         pesi.risk.factors <- rbind(pesi.risk.factors,solution$solution)
      } else {
         pesi.risk.factors <- matrix(solution$solution, nrow=1)
         dimnames(pesi.risk.factors) <- list(NULL,Ticker)
      }

      # Salva il vettore dei pesi del risk free (se necessario)
      if (desiderata.moneta.riferimento) {
         if (exists("pesi.rf")) {
            somma <- 1 - sum(solution$solution[indice.az.in.mon.rif])
            somma <- somma - sum(solution$solution[indice.fx])
            pesi.rf <- c(pesi.rf,somma)
         } else {
            somma <- 1 - sum(solution$solution[indice.az.in.mon.rif])
            somma <- somma - sum(solution$solution[indice.fx])
            pesi.rf <- somma
         }
      }

      # Calcolo dei pesi per i risk free nelle diverse valute non di riferimento
      # a) calcola la somma sulle branche
      if (length(indice.fx)>0) {
         if (exists("sum.pesi.branche.non.mon.rif")) {
            sum.pesi.branche.non.mon.rif <- cbind(sum.pesi.branche.non.mon.rif,rep(0,sum(indice.fx)))
            pesi.fx.non.mon.rif <- cbind(pesi.fx.non.mon.rif,rep(0,sum(indice.fx)))
         } else {
            sum.pesi.branche.non.mon.rif <- matrix(nrow=sum(indice.fx),ncol=1)
            dimnames(sum.pesi.branche.non.mon.rif) <- list(elenco.monete,NULL)
            pesi.fx.non.mon.rif <- matrix(nrow=sum(indice.fx),ncol=1)
            dimnames(pesi.fx.non.mon.rif) <- list(elenco.monete,NULL)
         }
         i <- dim(sum.pesi.branche.non.mon.rif)[2]
         for (moneta in elenco.monete) {
            sum.pesi.branche.non.mon.rif[moneta,i] <- sum(solution$solution[indice.branche.per.monete[moneta,]])
            pesi.fx.non.mon.rif[moneta,i] <- sum(solution$solution[indice.monete.fx[moneta,]])
         }
      }

      # Salva il vettore dei pesi delle azioni in una matrice
      if (nb.branche > 0 ) {
         if (exists("pesi.az")) {
            pesi.az <- rbind(pesi.az,solution$solution[indice.az])
         } else {
            pesi.az <- matrix(solution$solution[indice.az], nrow=1)
            dimnames(pesi.az) <- list(NULL,Ticker[indice.az])
         }
      }

      # Salva il vettore dei pesi degli fx in una matrice
      if (nb.fx > 0) {
         if (exists("pesi.fx")) {
            pesi.fx <- rbind(pesi.fx,solution$solution[indice.fx])
         } else {
            pesi.fx <- matrix(solution$solution[indice.fx], nrow=1)
            dimnames(pesi.fx) <- list(NULL,Ticker[indice.fx])
         }
      }
      rm(solution)
   } # fine for


   # Determina il minimo rispetto alla varianza nella curva media-varianza
   sigma.minimo <- min(sigma)
   indice.valori.efficienti <- mu >= mu[sigma==sigma.minimo]

   risultati <- list(mu = mu[indice.valori.efficienti],sigma=sigma[indice.valori.efficienti],VaRisk=VaRisk[indice.valori.efficienti],soluzioni=lista.soluzioni[indice.valori.efficienti],pesi.risk.factors=pesi.risk.factors[indice.valori.efficienti,])

   if (exists("pesi.rf")) {
      risultati[["pesi.rf"]] = pesi.rf[indice.valori.efficienti]
   }

   if (length(indice.fx)>0) {
      pesi.rf.non.mon.rif <- pesi.fx.non.mon.rif - sum.pesi.branche.non.mon.rif
      risultati[["pesi.rf.non.mon.rif"]] = pesi.rf.non.mon.rif[indice.valori.efficienti]
   }

   if (exists("pesi.az")){
      risultati[[length(risultati)+1]] = pesi.az[indice.valori.efficienti]
   }

   if (exists("pesi.fx")) {
      risultati[[length(risultati)+1]] = pesi.fx[indice.valori.efficienti]
   }

} # fine if (continua)

rm (risultato.query)

# Rimuovi i risultati precedenti
stringa = paste("DELETE FROM R_risultati_ottimizzazione_per_cliente WHERE Cliente LIKE '", cliente,"'",sep="")
risultato.query <- sqlQuery(channel.markowitz, stringa)
rm (risultato.query)

for (i in 1:length(mu)) {
   for (j in 1:nb.risk.factors) {
      if (length(grep("_Moneta",Ticker[j])) == 0) {
         stringa = "INSERT INTO R_risultati_ottimizzazione_per_cliente (cliente, Mu, Sigma, VaRisk, Ticker, Peso) VALUES ("
         stringa = paste(stringa,"'",cliente,"'",sep="")
         stringa = paste(stringa,mu[i],sigma[i],VaRisk[i],sep=",")
         stringa = paste(stringa,",","'",Ticker[j],"'",sep="")
         stringa = paste(stringa,pesi.risk.factors[i,j],sep=",")
         stringa = paste(stringa,")",sep="")
         risultato.query <- sqlQuery(channel.markowitz, stringa)
         rm (risultato.query)
      } else {
         if (exists("moneta.rf.non.mon.rif")) {  #ci sono risk free e per cui occorre fare un loop su tutte le monete <> moneta rif.
            moneta <- substr(Ticker[j],1,3)
            for (mon in moneta.rf.non.mon.rif) {
               if (mon==moneta) { #esiste il rf nella moneta desiderata
                  stringa = "INSERT INTO R_risultati_ottimizzazione_per_cliente (cliente, Mu, Sigma, VaRisk, Ticker, Peso) VALUES ("
                  stringa = paste(stringa,"'",cliente,"'",sep="")
                  stringa = paste(stringa,mu[i],sigma[i],VaRisk[i],sep=",")
                  stringa = paste(stringa,",","'",moneta,"_Moneta","'",sep="")
                  stringa = paste(stringa,pesi.rf.non.mon.rif[moneta,i],sep=",")
                  stringa = paste(stringa,")",sep="")
                  risultato.query <- sqlQuery(channel.markowitz, stringa)
                  rm (risultato.query)
                  break
               }
            }
         }
      }
   }

   # Se il risk free in moneta di riferimento è desiderato allora introduci anche il peso per il risk free
   if (desiderata.moneta.riferimento) {
      stringa = "INSERT INTO R_risultati_ottimizzazione_per_cliente (cliente, Mu, Sigma, VaRisk, Ticker, Peso) VALUES ("
      stringa = paste(stringa,"'",cliente,"'",sep="")
      stringa = paste(stringa,mu[i],sigma[i],VaRisk[i],sep=",")
      stringa = paste(stringa,",","'",moneta.riferimento,"_Moneta","'",sep="")
      stringa = paste(stringa,pesi.rf[i],sep=",")
      stringa = paste(stringa,")",sep="")
      risultato.query <- sqlQuery(channel.markowitz, stringa)
      rm (risultato.query)
   }
}

# Stampa i grafici desiderati
png(filename = "c:/R/MediaVarianza.png", width = 600, height = 450, pointsize = 12)
plot(sigma[indice.valori.efficienti],mu[indice.valori.efficienti],main=paste("Media-Varianza per ",cliente),type="l",ylab="E[r]",xlab=expression(sigma),lwd=2)
dev.off()

png(filename = "c:/R/VARMedia.png", width = 600, height = 450, pointsize = 12)
plot(-VaRisk[indice.valori.efficienti],mu[indice.valori.efficienti],main=paste("E[R]-Var per ",cliente),ylab="Expected Return",xlab="VAR",type="l",lwd=2)
dev.off()

save.image(file = paste(cliente,".RData",sep=""), safe = FALSE)
#print ("Terminato")
#quit(save="no")
  }