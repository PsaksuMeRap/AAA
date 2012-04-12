# TODO: Add comment
# 
# Author: claudio
###############################################################################

library("RODBC")
.utente = "sa"
.password = "ghD54+J*x"

db.prezziStoriciVAR = odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
