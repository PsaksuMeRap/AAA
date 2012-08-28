# TODO: Add comment
# 
# Author: Claudio
###############################################################################

# portafoglio gabriele ponti

cc <- new("Conto_corrente",currency=new("Currency","EUR"),
		name="EUR-eur",id=new("IdCharacter","Conto corrente EUR"))
positionCc <- new("PositionConto_corrente",
		id=cc@id,security=cc,quantity=toMoney(7085000,"EUR"),value=toMoney(7085000,"EUR"))
		
positions <- new("Positions",list(positionCc))

portfolio <- new("Portfolio",owner="asymmetricEquity",referenceCurrency=new("Currency","EUR"),positions)

saveTestPortfolio(portfolio,portfolioSubDirectory="asymmetricEquity")


# portafoglio sandro maggi
cc <- new("Conto_corrente",currency=new("Currency","EUR"),
		name="EUR-eur",id=new("IdCharacter","Conto corrente EUR"))
positionCc <- new("PositionConto_corrente",
		id=cc@id,security=cc,quantity=toMoney(33905000,"EUR"),value=toMoney(33905000,"EUR"))

positions <- new("Positions",list(positionCc))

portfolio <- new("Portfolio",owner="pippo250",referenceCurrency=new("Currency","EUR"),positions)

saveTestPortfolio(portfolio,portfolioSubDirectory="multistrategy")