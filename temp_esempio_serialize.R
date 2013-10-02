library("DBI")
library("RPostgreSQL")

# create an PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL")

# open the connection using user, passsword, etc., as
con <- dbConnect(drv, host="192.168.1.44",dbname = "Rtest",
		user="postgres",password="mamma+1postgres")


a <- list(nome="Claudio",cognome="Ortelli",eta=45)
b <- rawToChar(serialize(a,NULL,ascii=TRUE))


command <- "INSERT INTO objects (nome,object) VALUES('Oggetto1',"
command <- paste(command,"'",b,"');",sep="")
res <- dbSendQuery(con, command)
res <- dbGetQuery(con, "SELECT * from objects;")
d <- unserialize(charToRaw(res$object))





