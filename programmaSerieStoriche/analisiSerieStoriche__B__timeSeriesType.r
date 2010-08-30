## definisci le funzioni necessarie per i widget sottostanti

onTsType <- function() {
   ## prendi il valore attuale
   desiredType <- r.tsType[["get.selection"]]()

   ## costruisci il dataframe delle serie storiche da utilizzare per
   ## il report o i grafici
   if (desiredType=="Levels")
   {  
      dates <<- dates_orig
      nbObs <<- length(dates)
      ts.df <<- ts.df_orig
    }

   if (desiredType=="Log-returns")
   {
      dates <<- dates_orig[-1]
      nbObs <<- length(dates)
      ts.df <<- log(ts.df_orig[-1,]) - log(ts.df_orig[1:nbObs,])
   }

   if (desiredType=="Returns")
   {
      dates <<- dates_orig[-1]
      nbObs <<- length(dates)
      ts.df <<- (ts.df_orig[-1,] - ts.df_orig[1:nbObs,]) / ts.df_orig[1:nbObs,]
   }

   plotFrom <<- 1
   plotTo <<- nbObs

   if (withDates)
   {
     dataInizio <<- as.character(min(dates))
     dataFine <<- as.character(max(dates))
   } else {
     dataInizio <<- 1
     dataFine <<- nbObs
   }
   etichettaDataInizio <<- paste("Start:",dataInizio)
   sliderDataIniziale[["set.value"]](0)
   dateStart.l[["set.label"]](value=etichettaDataInizio)

   etichettaDataFine = paste(  "End:  ",dataFine)
   sliderDataFinale[["set.value"]](1)
   dateEnd.l[["set.label"]](value=etichettaDataFine)

   return()
}

