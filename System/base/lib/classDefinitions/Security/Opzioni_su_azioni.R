# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Opzioni_su_azioni",representation(currency="Currency",name="character",id="Id",underlying="Underlying",
				expiryDate="character",strike="numeric"),contains="Security") 

