# TODO: Add comment
# 
# Author: claudio
###############################################################################

library("RODBC")
.utente = "sa"
.password = "ghD54+J*x"

db.prezziStoriciVAR = odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)

#db.tassiStoriciVAR = odbcConnect("tassi_storici_VAR","sa","ghD54+J*x")
#db.tempdb = odbcConnect("tempdb","sa","ghD54+J*x")

#db.prezziStoriciVAR = odbcConnect("prezzi_storici_azioni_VAR")
#db.tassiStoriciVAR = odbcConnect("tassi_storici_VAR")
#db.tempdb = odbcConnect("tempdb")
