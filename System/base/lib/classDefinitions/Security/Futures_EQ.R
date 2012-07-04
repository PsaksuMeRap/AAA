# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Futures_EQ",representation(currency="Currency",name="character",id="Id",underlying="Underlying",deliveryDate="character",deliveryPrice="Money"),contains="Security") 

