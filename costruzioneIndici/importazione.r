## questo file contiene le seguenti funzioni
## 1) filtroValuta
## 2) importaHedgeFunds (utilizza filtroValuta)
##   2.1) previousEOM
##   2.2) computePrices


filtroValuta <- function(AUM.df,verbose=TRUE) {
  ## INPUT
  ## AUM.df è il data.frame con i dati dei fogli AUM_XXX.csv che contengono
  ## tre colonne: c("Nome","AUM","Currency")

  ## OUTPUT
  ## l.filtroValuta è una named list le cui componenti sono una lista:
  ## l.filtroValuta -- 
  ##                1) list --
  ##                        1) valuta: character 
  ##                        2) filtro: vector TRUE/FALSE
  ##                2) list -- ...
   
  ## DESCRIZIONE
  ## questa funzione restituisce una lista "l.filtroValuta" le cui componenti hanno il 
  ## nome corrispondente alle monete considerate e sono a loro volta delle liste a due componenti:
  ## $valuta: contiene il nome della moneta per cui si desidera applicare il filtro. 
  ## $filtro: è un vettore di TRUE/FALSE della medesima lunghezza di "AUM.df". L'i-esima
  ##          componente indica se la i-esima riga del data.frame "AUM.df" 
  ##          contiene un fondo della valuta corrispondente.


  ## ! SOLO LE MONETE NEL CODICE SEGUENTE SONO CONSIDERATE.

  ## identifica quali records di AUM.df sono della valuta indicata 
  v.usd <- AUM.df[,"Currency"] == "USD" | AUM.df[,"Currency"] == "usd"
  v.eur <- AUM.df[,"Currency"] == "EUR" | AUM.df[,"Currency"] == "eur"
  v.gbp <- AUM.df[,"Currency"] == "GBP" | AUM.df[,"Currency"] == "gbp"
  v.chf <- AUM.df[,"Currency"] == "CHF" | AUM.df[,"Currency"] == "chf"
  v.jpy <- AUM.df[,"Currency"] == "JPY" | AUM.df[,"Currency"] == "jpy"
  v.sek <- AUM.df[,"Currency"] == "SEK" | AUM.df[,"Currency"] == "sek"
  v.aud <- AUM.df[,"Currency"] == "AUD" | AUM.df[,"Currency"] == "aud" 
  v.cad <- AUM.df[,"Currency"] == "CAD" | AUM.df[,"Currency"] == "cad"
  v.nok <- AUM.df[,"Currency"] == "NOK" | AUM.df[,"Currency"] == "nok" 
  v.pln <- AUM.df[,"Currency"] == "PLN" | AUM.df[,"Currency"] == "pln"
  v.hkd <- AUM.df[,"Currency"] == "HKD" | AUM.df[,"Currency"] == "hkd"
  v.ils <- AUM.df[,"Currency"] == "ILS" | AUM.df[,"Currency"] == "ils"
  v.zar <- AUM.df[,"Currency"] == "ZAR" | AUM.df[,"Currency"] == "zar" 

  ## raggruppa i vettori in una lista
  l.filtroValuta <- list(usd=list(valuta="usd",filtro=v.usd),
                         eur=list(valuta="eur",filtro=v.eur),
                         gbp=list(valuta="gbp",filtro=v.gbp),
                         chf=list(valuta="chf",filtro=v.chf),
                         jpy=list(valuta="jpy",filtro=v.jpy),
                         sek=list(valuta="sek",filtro=v.sek),
                         aud=list(valuta="aud",filtro=v.aud),
                         cad=list(valuta="cad",filtro=v.cad),
                         nok=list(valuta="nok",filtro=v.nok),
                         pln=list(valuta="pln",filtro=v.pln),
                         hkd=list(valuta="hkd",filtro=v.hkd),
                         ils=list(valuta="ils",filtro=v.ils),
                         zar=list(valuta="zar",filtro=v.zar)
                         )

  if (sum(c(v.usd,v.eur,v.gbp,v.chf,v.jpy,v.sek,v.aud,v.cad,v.nok,v.pln,v.hkd,v.ils,v.zar)) != nrow(AUM.df)) {
    print ("Attenzione mancano valute!")
    print( AUM.df[!(v.usd | v.eur | v.gbp | v.chf | v.jpy | v.sek | v.aud | v.cad | v.nok | v.pln | v.hkd | v.ils | v.zar ),] )
    return()
  }
  rm(v.usd,v.eur,v.gbp,v.chf,v.jpy,v.sek,v.aud,v.cad,v.nok,v.pln,v.hkd,v.ils,v.zar)
  if (verbose) {
    print("----------------------------------------")
    for (f in l.filtroValuta) {
      print ( paste("Ci sono",sum(f$filtro),"fondi con valuta",f$valuta) )
    }
    print("----------------------------------------")
  }
  class(l.filtroValuta) <- "filtroValuta"
  return(l.filtroValuta)
}

summary.filtroValuta <- function(myList) {
  for (j in myList) {
     if (sum(j$filtro)>0) print(paste(j$valuta,": ",sum(j$filtro),sep=""))
   }
}



importaHedgeFunds <- function(estensione,valuteDaAggiustare=c("usd","eur","gbp","chf","cad","jpy","aud"),soloValute=valuteDaAggiustare,verbose=TRUE) {
  ## INPUT
  ## estensione: è l'estensione utilizzata per identificare i file da importare.
  ##             Sono importati i seguenti files:
  ##             1) File dei rendimenti. Il formato del file deve essere csv ed
  ##                avere nome uguale a "returns_" + estensione + ".csv"
  ##                La colonna A contiene i nomi dei fondi, la riga 1 le date, 
  ##                la casella A1 è vuota.
  ##             2) File con gli AUM. Il formato del file deve essere csv ed 
  ##                avere nome uguale a "AUM_" + estensione + ".csv"
  ##                La colonna A contiene i nomi dei fondi, la B gli AUM, la C 
  ##                la moneta. Le colonne non hanno intestazione.
  ##             3) File con i tassi a 1 mese nelle diverse monete. Il nome 
  ##                deve essere uguale a "tassi interesse per hedge fonds.csv"
  ##                e il formato è csv. La prima colonna contiene la data di 
  ##                inizio mese, la seconda la data di fine mese, poi seguono
  ##                i valori dei tassi d'interesse a 1 mese nelle diverse 
  ##                monete. I valori corrispondono ai tassi osservati alla prima
  ##                data (inizio mese). 

  ## OUTPUT
  ## ritorna una lista a tre componenti:
  ##    1)  matrix: la matrice dei prezzi come calcolata nella variabile prezzi
  ##    2)  AUM.df: il data.frame datiAUM 
  ##    3)  rendimentoForward.df: il tasso forward utilizzato per correggere i 
  ##                     rendimenti dei fondi non in usd. Contiene il logaritmo
  ##                     naturale di (1+r_usd/12) / (1+r_xxx/12) e corrisponde 
  ##                     al logaritmo naturale di (1 + rendimento % atteso sul 
  ##                     cambio)

  importaRendimentiPercentuali <- function(fileName) {

    ## importa solo le date (prima riga) 
    data <- read.csv(file=fileName, header=FALSE,nrows=1,colClasses="character")
    data <- unlist(data)
    data <- as.Date(data, "%m/%d/%y")

    ## elimina la prima colonna che non contiene una data
    data <- data[-1]

    ## importa i rendimenti (escludi la prima riga con la data)
    rendimenti <- read.csv(file=fileName, header=FALSE, skip=1, stringsAsFactors=FALSE)

    ## crea il vettore coi nomi dei fondi
    nomiFondi <- rendimenti[[1]]
    
    ## assegna ai rownames dei rendimenti il nome del fondo
    rownames(rendimenti) <- nomiFondi
    rendimenti <- rendimenti[,-1]

    ## assegna ai colnames dei rendimenti le date
    colnames(rendimenti) <- data

    ## trasponi e converti il data.frame rendimenti in una matrice
    rendimenti <- t(as.matrix(rendimenti))

    return(list(data=data,rendimenti=rendimenti))
  }

  previousEOM <- function(x) {
    ## this function returns the "end of month date" of the month previous to date x. 
    ## dates in the vector x must be in the format yyyy-mm-dd
    date <- as.Date( paste(format(x,"%Y-%m"),"-01",sep="") )
    return(seq(date,length.out=2,by="-1 day")[2])
  }

  importaDifferenzialeTassi <- function(fileName) {
    ## importa il differenziale dei Tassi d'interesse per la correzione dei rendimenti non in
    ## usd
    
    differenziale <- read.csv(fileName)
    colnames(differenziale) <- c("Data da eliminare","Data","gbp","chf","eur","jpy","cad","aud")
    differenziale <- differenziale[,-1]

    ## differenziale contiene il logaritmo naturale di (1+r_usd/12) / (1+r_xxx/12) e corrisponde 
    ## al logaritmo naturale di (1 + rendimento atteso sul cambio). In realtà non sarebbe stato
    ## necessario calcolare il logaritmo ... :)
    differenziale[,-1] <- exp(differenziale[,-1]) - 1
    return(differenziale)
  }

  importaAUM <- function(fileName) {
    ## importa gli ASSET UNDER MANAGEMENT e le monete di riferimento dei fondi
    datiAUM <- read.csv(file=fileName, header=FALSE, stringsAsFactors=FALSE)
    colnames(datiAUM) <- c("Nome","AUM","Currency")
    rownames(datiAUM) <- datiAUM[,"Nome"]
    trasformaMonete <- c("usd","eur","gbp","chf","jpy","sek","aud","cad","nok","pln","hkd","ils","zar","jpy")
    names(trasformaMonete) <- c("USD","EUR","GBP","CHF","JPY","SEK","AUD","CAD","NOK","PLN","HKD","ILS","ZAR","jpy")
    datiAUM[,"Currency"] <- trasformaMonete[ datiAUM[,"Currency"] ]
    return(datiAUM)
  }

  verificaNomiFondi <- function(datiAUM,returns) {
    ## verifica che i fondi provenienti dal file con gli AUM e i rendimenti 
    ## siano gli stessi
    nomiSerieAUM <- datiAUM[order(datiAUM[,"Nome"]),"Nome"]
    nomiSerieMatrix <- colnames(returns)[order(colnames(returns))]
    notEq <- nomiSerieAUM != nomiSerieMatrix
    if (sum(notEq) > 0) {
      print ("Le serie AUM seguenti sono diverse:")
      print (nomiSerieAUM[notEq]) 
    }    
  }

  calcolaHedgedReturns <- function(rendimenti, datiAUM, differenzialeTassi) {
    nbObs <- nrow(rendimenti)
    for (hedgeFond in colnames(rendimenti)) {
      moneta = datiAUM[hedgeFond,"Currency"]
      if (is.element(moneta,setdiff(valuteDaAggiustare,"usd"))) {
	rendimenti[,hedgeFond] <- (1+rendimenti[,hedgeFond]) * (1+differenzialeTassi[1:nbObs,moneta]) - 1
      }
    }
    return(rendimenti)
  }

  trasformaRendimentiInPrezzi <- function(rendimenti) {
    computePrices <- function(x) {
      ## this function computes the prices given the % returns
      ## la prima componente deve essere NA_real, in quanto il prezzo
      ## al tempo t=0 non è osservato

      nbObs = length(x)
      validReturns <- !is.na(x)
      if (!any(validReturns)) return(x)

      start = min( (1:nbObs)[validReturns] ) - 1
      y <- x
      y[start] <- 100.0
      for (i in (start+1):nbObs) {
	y[i] = y[i-1]*(1+x[i])
      }
      return(y)
    }

    ## trasforma i rendimenti % in prezzi. La prima osservazione <> NA sarà 100
    firstDate <- as.character( previousEOM(as.Date(rownames(rendimenti)[1])) )

    tmp <- rep(NA_real_,ncol(rendimenti))
    prezzi <- rbind(tmp,rendimenti)
    rownames(prezzi)[1] <- firstDate
    rm(tmp,firstDate)
    for (j in 1:ncol(prezzi)) {
      prezzi[,j] <- computePrices(prezzi[,j])
    }
    return(prezzi)
  }

  ## Inizio procedura 
  workingDirectory = "/home/claudio/Dropbox/analisi serie storiche/capitalgest/dati/"
  tmp <- paste(workingDirectory,"returns_",estensione,".csv",sep="")

  datiTmp <- importaRendimentiPercentuali(fileName=tmp); rm(tmp)
  data <- datiTmp$data
  rendimenti <- datiTmp$rendimenti; rm(datiTmp)
 
  
  ## importa i tassi d'interesse necessari per calcolare i costi di hedge
  tmp <- paste(workingDirectory,"tassi interesse per hedge fonds.csv",sep="")
  differenzialeTassi <- importaDifferenzialeTassi(fileName=tmp); rm(tmp)

  ## importa gli Asset Under Management e le monete di riferimento dei fondi
  tmp <- paste(workingDirectory,"AUM_",estensione,".csv",sep="")
  datiAUM <- importaAUM(fileName=tmp); rm(tmp)

  if (verbose) filtroValuta(datiAUM,verbose=verbose)

  verificaNomiFondi(datiAUM,returns=rendimenti)

  ## aggiusta i rendimenti delle monete non in usd per i costi dell'hedge
  rendimenti <- calcolaHedgedReturns(rendimenti, datiAUM, differenzialeTassi)

  prezzi <- trasformaRendimentiInPrezzi(rendimenti)


  ## considera solo le valute desiderate
  if (soloValute[1] != "tutte") {
    daConsiderare <- is.element(datiAUM[,"Currency"],soloValute)
    prezzi <- prezzi[,daConsiderare]
    datiAUM=datiAUM[daConsiderare,]
  }
 
  return(list(matrix=prezzi,AUM.df=datiAUM,rendimentoForward.df=differenzialeTassi))
} ## fine funzione importaHedgeFunds


importLyxorIndices <- function(dataFile) {

    ## importa solo i nomi delle serie (prima riga) 
    nomiSerie <- read.csv(file=dataFile, header=FALSE,nrows=1,colClasses="character")
    nomiSerie <- unlist(nomiSerie)
    names(nomiSerie) <- NULL

    ## elimina la prima componente che contiene "INDEX"
    nomiSerie <-  nomiSerie[-1]
   
    ## importa i livelli (escludi la prima riga con gli header)
    livelli.df <- read.csv(file=dataFile, header=FALSE, skip=1, stringsAsFactors=FALSE)
    
    yyyymmdd <- as.Date(livelli.df[[1]], format="%m/%d/%Y")
    # yyyymmdd <- format(yyyymmdd,"%Y-%m-%d")

    ## crea il vettore coi nomi dei fondi
    livelli.df <- livelli.df[,-1,drop=FALSE]
    
    ## assegna i nomi dei fondi
    colnames(livelli.df) <- nomiSerie

    ## assegna ai rownames le date
    rownames(livelli.df) <- yyyymmdd

    return(livelli.df)
  }
