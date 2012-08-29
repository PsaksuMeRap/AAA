# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# create the ID and MonetaInvestimento vectors to populate the data.frame
ID <- c("fixedIncome","globalEquity","globalEconomy","multistrategy","asymmetricEquity")
MonetaInvestimento <- c("EUR","CHF","CHF","EUR","EUR")

# create the data.frame
politicaInvestimento.df <- data.frame(ID=ID,MonetaInvestimento=MonetaInvestimento)

# create the instrument repository		
politicaInvestimento <- create_repositoryPoliticaInvestimento(politicaInvestimento.df=politicaInvestimento.df) 

# assegna i repositories
assign("politicaInvestimento",politicaInvestimento,envir=repositories)
