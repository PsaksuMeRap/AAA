# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("FX_Forward",representation(currency="Currency",name="character",id="Id",deliveryDate="character",deliveryPrice="Money"),contains="Security") 

