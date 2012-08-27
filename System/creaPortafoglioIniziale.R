# TODO: Add comment
# 
# Author: Claudio
###############################################################################

# portafoglio gabriele ponti

cc <- new("Conto_corrente",currency=new("Currency","EUR"),
		name="EUR-eur",id=new("IdCharacter","Conto corrente EUR"))
positionCc <- new("PositionConto_corrente",
		id=cc@id,security=cc,quantity=toMoney(15000000,"EUR"),value=toMoney(15000000,"EUR"))
		
positions <- new("Positions",list(positionCc))

portfolio <- new("Portfolio",referenceCurrency=new("Currency","EUR"),positions)


save_testPortfolio(portfolio,portfolioName="asymmetricEquity")


# portafoglio sandro maggi
cc <- new("Conto_corrente",currency=new("Currency","EUR"),
		name="EUR-eur",id=new("IdCharacter","Conto corrente EUR"))
positionCc <- new("PositionConto_corrente",
		id=cc@id,security=cc,quantity=toMoney(15000000,"EUR"),value=toMoney(15000000,"EUR"))

positions <- new("Positions",list(positionCc))

portfolioGabriele <- new("Portfolio",referenceCurrency=new("Currency","EUR"),positions)