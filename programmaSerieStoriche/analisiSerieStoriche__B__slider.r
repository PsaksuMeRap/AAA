## definisci le funzioni necessarie per i widget sottostanti

onSliderDataIniziale <- function(...) {

  x <- as.numeric(unlist(...)[1])

  ## verifica che non sia superiore a quello della data finale
  xDataFinale <- sliderDataFinale[["get.value"]]()
  plotFrom <<- max(1,round(nbObs*x))
  newStartDate <- dates[plotFrom]
  dateStart.l[["set.label"]](value=paste("Start:",newStartDate))


  if (x > xDataFinale) 
    {
      sliderDataFinale[["set.value"]](x)
      plotTo <<- plotFrom
      dateEnd.l[["set.label"]](value=paste("End:  ",newStartDate))
    }
}

onSliderDataFinale <- function(...) {

  x <- as.numeric(unlist(...)[1])

  ## verifica che non sia superiore a quello della data finale
  xDataIniziale <- sliderDataIniziale[["get.value"]]()
  plotTo <<- max(1,round(nbObs*x))
  newEndDate <- dates[plotTo]
  dateEnd.l[["set.label"]](value=paste("End:  ",newEndDate))


  if (x < xDataIniziale) 
    {
      sliderDataIniziale[["set.value"]](x)
      plotFrom <<- plotTo
      dateStart.l[["set.label"]](value=paste("Start:",newEndDate))
    }
}

onStartPlus <- function() {
  plotFrom <<- min(plotFrom + 1,nbObs,plotTo)
  sliderDataIniziale[["set.value"]](plotFrom/nbObs)
  newStartDate <- dates[plotFrom]
  dateStart.l[["set.label"]](value=paste("Start:",newStartDate))
}

onStartMinus <- function() {
  plotFrom <<- max(plotFrom - 1,1)
  sliderDataIniziale[["set.value"]](plotFrom/nbObs)
  newStartDate <- dates[plotFrom]
  dateStart.l[["set.label"]](value=paste("Start:",newStartDate))
}

onEndPlus <- function() {
  plotTo <<- min(plotTo + 1,nbObs)
  sliderDataFinale[["set.value"]](plotTo/nbObs)
  newEndDate <- dates[plotTo]
  dateEnd.l[["set.label"]](value=paste("End:  ",newEndDate))
}

onEndMinus <- function() {
  plotTo <<- max(plotTo - 1,1,plotFrom)
  sliderDataFinale[["set.value"]](plotTo/nbObs)
  newEndDate <- dates[plotTo]
  dateEnd.l[["set.label"]](value=paste("End:  ",newEndDate))
}

## crea i dataframe contenenti i widgets per le date
dateFrame <- ttkframe(parent=topWindow)
dateFrameStart <- ttkframe(parent=dateFrame)
dateFrameEnd <- ttkframe(parent=dateFrame)

## crea le etichette con la data iniziale e finale
dateStart.l <- create_label(parent=dateFrameStart,value=etichettaDataInizio)
dateEnd.l <- create_label(parent=dateFrameEnd,value=etichettaDataFine)
## crea gli slider della data iniziale e finale
sliderDataIniziale <- create_ttkslider(parent=dateFrame,orient="horizontal",
                          from=0,to=1,length=425,initialValue=0,command=onSliderDataIniziale)

sliderDataFinale <- create_ttkslider(parent=dateFrame,orient="horizontal",
                          from=0,to=1,length=425,initialValue=1,command=onSliderDataFinale)
## crea i bottoni di incremento e decremento della data iniziale e finale
startPlus.b <- create_button(parent=dateFrameStart,text="+",width=3,command=onStartPlus)
startMinus.b <- create_button(parent=dateFrameStart,text="-",width=3,command=onStartMinus)
endPlus.b <- create_button(parent=dateFrameEnd,text="+",width=3,command=onEndPlus)
endMinus.b <- create_button(parent=dateFrameEnd,text="-",width=3,command=onEndMinus)



## posiziona i widgets all'interno dei propri frame
tkgrid(dateStart.l[["label"]],startPlus.b[["button"]],startMinus.b[["button"]],padx=10,pady=10,sticky="w")
tkgrid(dateEnd.l[["label"]],endPlus.b[["button"]],endMinus.b[["button"]],padx=10,pady=10,sticky="w")


## posiziona i frame e i widgets
tkgrid(dateFrameStart,sticky="w")
tkgrid(sliderDataIniziale[["slider"]],padx=10,pady=10)
tkgrid(dateFrameEnd  ,sticky="w")
tkgrid(sliderDataFinale[["slider"]],padx=10,pady=10)
