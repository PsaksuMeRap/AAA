# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Opzioni_su_divise",representation(currency="Currency",name="character",id="Id",optionType="character",underlying="Underlying",
				expiryDate="character",strike="numeric"),contains="Security") 

#optionType = "Put" or "Call"
