# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setClass("ID_Ayrton",representation(ID_AAA="numeric",ID_strumento="numeric"))
setClassUnion("ID", "ID_Ayrton")
s
setClass("Security",contains="VIRTUAL")
setClass("Equity",representation(name="character",ID="ID"),contains="Security") 

