

path <- "/home/claudio/Dropbox/alphadropbox/Lyxor hedge funds/"
csvFile <- "LYXOR Indices Track Total new.csv"
dataFile <- paste(path,csvFile,sep="")

constituentsPrices.l <- importaPrezziDaCsv(dataFile)

#singleSeries <- extractTimeSeries("COMPOSITE",constituentsPrices.l)
#plot(singleSeries$data.v,type="l")

simpleReportTimeSeries(constituentsPrices.l)

numeroFondi <- length(constituentsPrices.l)
pesi.v <- rep(1/numeroFondi, numeroFondi)
names(pesi.v) <- extractTimeSeriesNames(constituentsPrices.l)

tmp <- initializeIndexByWeights(startingDate="2002-04-02",weights=pesi.v,constituentsPrices.l)

