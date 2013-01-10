# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Opzioni_su_obbligazioni",representation(currency="Currency",name="character",id="Id",optionType="character",
				underlying="Underlying",expiryDate="character",strike="numeric"),contains="Security") 

# comment: optionType is "Put" or "Call"
