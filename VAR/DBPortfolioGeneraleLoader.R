# TODO: Add comment
# 
# Author: claudio
###############################################################################


## controllo tassi

interestRates <- create_repositoryInterestRates()
usdIR.df <- interestRates$rates.df[interestRates$rates.df[,"currency"]=="USD",]
orderAscending <- order(usdIR.df[,"maturity"])
usdIR.df <- usdIR.df[orderAscending,,drop=FALSE]
plot(usdIR.df[,"maturity"],usdIR.df[,"rate"])

approxfun(x=usdIR.df[,"maturity"],y=usdIR.df[,"rate"])





## importazione portafogli
dati.df <- DBPortfolioGeneraleLoader()

dati.df <- dati.df[,]

pp1 <- create_parserPositions()
posizioni <- pp1$parse(dati.df)

# crea una funzione che indica la classe di ogni posizione
t <- function(x) return(x$class())

a <- lapply(posizioni$positions,t)

pp2 <- create_parserPortfolio()
portfolio.CBGE <- pp2$parse("CB-ACC GLOBAL EQ",dati.df)


# rm(dati.df,pp1,pp2)


