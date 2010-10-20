sqlGetErrMsg <- function(channel)
  {
    msg <- odbcGetErrMsg(channel)
    if (length(msg)>0)
      {
        tmp <- paste(msg,collapse="\n")
        tkmessageBox(message=paste("ODBC Error:\n",tmp,type="ok",icon="error"))
        odbcClearError(channel)
        return(1)
      }
    odbcClearError(channel)
    return(0)
  }

set.query <- function(tableName, fieldNames)
  {
    if (missing(fieldNames))
    {
      query <- paste("SELECT * FROM",tableName, sep=' ')
    } else {
      query <- paste(fieldNames,collapse=",")
      query <- paste("SELECT",query,"FROM",tableName, sep=" ")
    }
    return(query)
  }


# returns information regarding the columnName, columnStorage and columnType
sqlColumnsInfo <- function(myConnection, tableName, columnNames)
{
  if (missing(columnNames)) return(sqlColumns(myConnection, tableName)[c(4,5,6)])
  return(sqlColumns(myConnection, tableName)[columnNames])
}

# return the list of all databases in a given instance of MS sql server
sqlDatabases <- function(myConnection)
  {
     databases <- sqlQuery(channel=myConnection,query="sp_databases",errors=FALSE,as.is=TRUE)
     error <- sqlGetErrMsg(channel=myConnection)     
     if (error) return(list(Database=""))
     tmp <- databases["DATABASE_NAME"]
     dimnames(tmp)[[2]] <- c("Database")
     return(tmp) 
  }

## return the working database of myConnection
sqlConnectionDatabase <- function(myConnection) UseMethod("sqlConnectionDatabase")
sqlConnectionDatabase.mssql <- function(myConnection)
  { 
    ## extract the name of the connection database  
    tmp <- attr(myConnection,wich="connection.string")
    tmp <- strsplit(tmp,";")
    tmp <- strsplit(tmp[[1]][4],"=")
    return(tmp[[1]][2])
  }
sqlConnectionDatabase.default <- function(myConnection)
  {
  tkmessageBox(message="sqlConnectionDatabase: the method for this class not implemented.",icon="error",type="ok")
  return()  
  }
  
## return the working database of myConnection
sqlWorkingDatabase <- function(myConnection) UseMethod("sqlWorkingDatabase")
sqlWorkingDatabase.mssql <- function(myConnection)
  { 
    ## return the name of the active database
    return(sqlQuery(channel=myConnection,query="SELECT DB_NAME()",as.is=TRUE)[1,1])
  }
sqlWorkingDatabase.default <- function(myConnection)
  {
  tkmessageBox(message="sqlWorkingDatabase: the method for this class not implemented.",icon="error",type="ok")
  return()  
  }


## set the working database and return the old one
sqlSetWorkingDatabase <- function(myConnection,newDatabase) UseMethod("sqlSetWorkingDatabase")
sqlSetWorkingDatabase.mssql <- function(myConnection,newDatabase)
  {

    if (missing(newDatabase)) newDatabase <- sqlConnectionDatabase(myConnection=myConnection)
    ## extract the actual database
    actualDatabase <- sqlWorkingDatabase(myConnection=myConnection)

    if (actualDatabase == newDatabase) return(actualDatabase)
    
    ## change the db
    newDatabase <- gsub("\\[|\\]","",newDatabase)
    sqlQuery(channel=myConnection,query=paste("USE [",newDatabase,"]",sep=""))      

    return(actualDatabase)
  }
sqlSetWorkingDatabase.default <- function(myConnection,newDatabase)
  {
  tkmessageBox(message="sqlSetWorkingDatabase: the method for this class not implemented.",icon="error",type="ok")
  return()  
  }


sqlTablesInfo <- function(myConnection,database,withSystemTables=FALSE)
  {
    ## as an alternative to sqlTables use this implementation
    ## extract the actual database
    tmp <- sqlWorkingDatabase(myConnection)
    emptyList <- list("Table owner"="","Table name"="","Table type"="")
    
     
    database <- gsub("\\[|\\]","",database)
    sqlQuery(channel=myConnection,query=paste("USE [",database,"]",sep=""))      
    error <- sqlGetErrMsg(channel=myConnection)     
    if (error) 
      {
        return(emptyList)
      }
    
    
    tables <- sqlQuery(channel=myConnection,query="sp_tables",as.is=TRUE)
    error <- sqlGetErrMsg(channel=myConnection)     
    if (error) 
      {
        return(emptyList)
      }      
    
    
    sqlQuery(channel=myConnection,query=paste("USE [",tmp,"]",sep=""))
    error <- sqlGetErrMsg(channel=myConnection)

    if (withSystemTables)
      {
        tmp <- tables[c("TABLE_OWNER","TABLE_NAME","TABLE_TYPE")]
        dimnames(tmp)[[2]] <- c("Table owner","Table name","Table type")
        return(tmp) 
      } 
    isTable <- tables[,"TABLE_TYPE"] == "TABLE"
    tmp <- tables[isTable,c("TABLE_OWNER","TABLE_NAME","TABLE_TYPE")]
    dimnames(tmp)[[2]] <- c("Table owner","Table name","Table type")
    return(tmp)
  }  
  
# returns information regarding the columnType
sqlColumnsTypesInfo <- function(myConnection, tablename, columnNames)
{
  if (missing(columnNames))
  {
    odbcTypes <- sqlColumns(myConnection, tablename)[,6]
    class(odbcTypes) <- class(myConnection)[2]
    return(odbcTypes)
  } else {
    info <- sqlColumns(myConnection, tablename)[,c(4,5,6)]
    dimnames(info)[[1]] <- info[,1]
    odbcTypes <- info[columnNames,3]
    class(odbcTypes) <- class(myConnection)[2]
    return(odbcTypes)
  }
}

## returns true if the table exists in the given Database
tableExist <- function(myConnection,tableName,databaseName,...) UseMethod("tableExist")
tableExist.default <- function(myConnection,tableName,databaseName,...)
  {
    tkmessageBox(message="tableExist.default: invalid class for connection!",icon="error",type="ok")
    return(FALSE)
  }
tableExist.mssql <- function(myConnection,tableName,databaseName,dbOwner="[dbo]")
  { 
    if(!missing(databaseName)) ## change database
      {
        ## extract the actual database
        initialDb <- sqlWorkingDatabase(myConnection)
    
        ## remove squared brackets if exist
        database <- gsub("\\[|\\]","",databaseName)

        sqlQuery(channel=myConnection,query=paste("USE [",database,"]",sep=""))      
        sqlGetErrMsg(channel=myConnection) 
      }

    query = paste("SELECT * from dbo.sysobjects WHERE id = object_id(N'",
                  dbOwner,".[",tableName,"]') AND OBJECTPROPERTY(id, N'IsUserTable')=1",sep="")
    
    result <- sqlQuery(myConnection,query)

    if (!missing(databaseName))
      {
        sqlQuery(channel=myConnection,query=paste("USE [",initialDb,"]",sep=""))
        sqlGetErrMsg(channel=myConnection)
      }
    
    if (nrow(result)>0) return(TRUE) else return(FALSE) 
  }


## returns a dataframe containing informations about the following properties of
## the columns of a table: "Column Name","Data Type","Length","Allow Nulls","Primary Key"
get.tableStructure <- function(myConnection,databaseName,tableName)
  {
    ## extract the actual database
    initialDb <- sqlWorkingDatabase(myConnection)
    
    ## remove squared brackets if exist
    database <- gsub("\\[|\\]","",databaseName)

    sqlQuery(channel=myConnection,query=paste("USE [",database,"]",sep=""))      
    sqlGetErrMsg(channel=myConnection) 
    
    df.tableStructure <- sqlColumnsInfo(myConnection,tableName,columnNames=c("COLUMN_NAME","TYPE_NAME","COLUMN_SIZE","NULLABLE"))
    tmp <- sqlPrimaryKeys(channel=myConnection,sqtable=tableName)
    sqlQuery(channel=myConnection,query=paste("USE [",initialDb,"]",sep=""))
    sqlGetErrMsg(channel=myConnection) 

    if (dim(tmp)[1]==0)
      {
        df.tableStructure[["PK"]] <- rep(FALSE,dim(df.tableStructure)[1])
      }
    else
      {
        df.tableStructure[["PK"]] <- is.element(df.tableStructure[,"COLUMN_NAME"],tmp[,"COLUMN_NAME"])
      }
    
    df.tableStructure[["NULLABLE"]] <- as.logical(df.tableStructure[,"NULLABLE"]) 
    dimnames(df.tableStructure)[[2]] <- c("Column Name","Data Type","Length","Allow Nulls","Primary Key")
    
    return(df.tableStructure)
  }
  

# questa funzione verifica che non ci siano nuovi tipi rispettivamente che
# vecchi tipi non siano stati eliminati
verifySqlTypes <- function(myConnection) UseMethod("verifySqlTypes")

verifySqlTypes.default <- function(myConnection)
{
  tkmessageBox(message="verifySqlTypes: invalid class for connection!",icon="error",type="ok")
  return(1)
}

verifySqlTypes.mssql <- function(myConnection)
{
  allowedTypes <- allowedSqlTypes(myConnection)
	availableTypes <- sqlTypeInfo(myConnection)[,1]
	if (identical(allowedTypes,availableTypes))
	{
		return(0)
	} else {
		tkmessageBox(message="verifySqlTypes: Types have changed!",icon="error",type="ok")
		return(1)
	}
}



# questa funzione restituisce il vettore dei nomi dei tipi di dati per i quali
# � garantita la conversione da odbc ad R. Essa fa uso di msSql.to.rType
allowedSqlTypes <- function(myConnection) UseMethod("allowedSqlTypes")

allowedSqlTypes.default <- function(myConnection)
{
  tkmessageBox(message="allowedSqlTypes: invalid class for connection!",icon="error",type="ok")
  return(NULL)
}

allowedSqlTypes.mssql <- function(myConnection)
{
  types <- c("sql_variant","uniqueidentifier","ntext","nvarchar","sysname","nchar","bit","tinyint","tinyint identity",
                            "bigint","bigint identity","image","varbinary","binary","timestamp","text","char","numeric","numeric() identity",
                            "decimal","money","smallmoney","decimal() identity","int","int identity","smallint","smallint identity","float",
                            "real","datetime","smalldatetime","varchar")
  class(types) <- "mssql"
	return(types)
}


# this function return a vector mapping the sqlTypes to "" or "'"
sqlNeedQuote <- function(myConnection) UseMethod("sqlNeedQuote")

sqlNeedQuote.mssql <- function(myConnection)
{
 types <- allowedSqlTypes(myConnection)
 sqlQuote <- c("'","'","'","'","'","'","","","","","","","'","'","'","'","'","","","",
               "","","","","","","","","","'","'","'")
 names(sqlQuote) <- types
 return(sqlQuote)
}

sqlNeedQuote.default <- function(myConnection)
{
  tkmessageBox(message="sqlNeedQuote: used default! Why? Should be 'mssql' or another.",icon="error",type="ok")
  return("")
}


mssql.to.rType.new <- function(types)
{
	# create the conversion vector containing the mapping from ms_sql_type -> R_type according
	# to to following table
  list(
	 "sql_variant"		    ="character",
	 "uniqueidentifier"	  ="character",
	 "ntext"		          ="character",
	 "nvarchar"		        ="character",
	 "sysname"		        ="character",
	 "nchar"		          ="character",
	 "bit"			          ="integer",
	 "tinyint"		        ="integer",
	 "tinyint identity"	  ="integer",
	 "bigint"		          ="integer",
	 "bigint identity"	  ="integer",
	 "image"		          ="character",
	 "varbinary"		      ="character",
	 "binary"		          ="character",
	 "timestamp"		      ="character",
	 "text"		            ="character",
	 "char"		            ="character",
	 "numeric"		        ="double",
	 "numeric() identity"	="double",
	 "decimal"		        ="character",
	 "money"		          ="character",
	 "smallmoney"		      ="character",
	 "decimal() identity"	="character",
	 "int"			          ="integer",
	 "int identity"	      ="integer",
	 "smallint"		        ="integer",
	 "smallint identity"	="integer",
	 "float"		          ="double",
	 "real"		            ="double",
	 "datetime"		        ="character",
	 "smalldatetime"	    ="character",
	 "varchar"		        ="character"
	)
	
  if (missing(types)) return(c("integer","double","character"))
  
	return(conversion[types])  # return a list

}
mssql.to.rType <- function(types)
{
	# create the conversion vector containing the mapping from ms_sql_type -> R_type according
	# to to following table

	# "sql_variant"		      "character"
	# "uniqueidentifier"	  "character"
	# "ntext"		            "character"
	# "nvarchar"		        "character"
	# "sysname"		          "character"
	# "nchar"		            "character"
	# "bit"			            "integer"
	# "tinyint"		          "integer"
	# "tinyint identity"	  "integer"
	# "bigint"		          "integer"
	# "bigint identity"	    "integer"
	# "image"		            "character"
	# "varbinary"		        "character"
	# "binary"		          "character"
	# "timestamp"		        "character"
	# "text"		            "character"
	# "char"		            "character"
	# "numeric"		          "double"
	# "numeric() identity"	"double"
	# "decimal"		          "character"
	# "money"		            "character"
	# "smallmoney"		      "character"
	# "decimal() identity"	"character"
	# "int"			            "integer"
	# "int identity"	      "integer"
	# "smallint"		        "integer"
	# "smallint identity"	  "integer"
	# "float"		            "double"
	# "real"		            "double"
	# "datetime"		        "character"
	# "smalldatetime"	      "character"
	# "varchar"		          "character"
	
  if (missing(types)) return(c("integer","double","character"))

  conversion <- c("character","character","character","character","character","character","integer","integer","integer","integer",
                     "integer","character","character","character","character","character","character","double","double","character",
                     "character","character","character","integer","integer","integer","integer","double","double","character",
                     "character","character")

	names(conversion) <- allowedSqlTypes.mssql()
	return(conversion[types])

}


# sqlTypeConversion: function used to map the names of sql types from a particular
# database engine to the corresponding R types
sqlTypeConversion <- function(sqlTypeNames) UseMethod("sqlTypeConversion")

sqlTypeConversion.mssql <- function(sqlTypeNames) return(mssql.to.rType(sqlTypeNames))

sqlTypeConversion.default <- function(sqlTypeNames)
{
  tkmessageBox(message="sqlTypeConversion: invalid class for sqlTypeNames!",icon="error",type="ok")
  return(NULL)
}


sqlToTclType <- function(types)
{
	# create the conversion vector containing the mapping from ms_sql_type -> tcl_type according
	# to to following table
  if (missing(types)) return(tclTypes <- c("logical","character","integer","double","date"))
  
	conversion <- c("character","character","character","character","character","character","logical","integer","integer","integer",
                     "integer","character","character","character","character","character","character","double","double","character",
                     "character","character","character","integer","integer","integer","integer","double","double","date",
                     "date","character")
	names(conversion) <- allowedSqlTypes.mssql()
	return(conversion[types])

}

# sqlToTclTypeConversion: function used to map the names of sql types from a particular
# database engine to the corresponding tcl types
sqlToTclTypeConversion <- function(sqlTypeNames) UseMethod("sqlTypeConversion")

sqlToTclTypeConversion.mssql <- function(sqlTypeNames) return(sqlToTclType(sqlTypeNames))


sqlToTclTypeConversion.default <- function(sqlTypeNames)
{
  tkmessageBox(message="sqlToTclTypeConversion: invalid class for sqlTypeNames!",icon="error",type="ok")
  return(NULL)
}


# questa funzione per il momento non serve ...
checkSqlTypeConversion <- function(typeVector)
{
	# check that all required types have been correctly converted to R types
	if (any(is.na(typeVector)))
	{
		tkmessageBox(message="checkSqlTypeConversion: NA values as types!",icon="error",type="ok")
		return(1)
	} else {
		return(0)
	}
}



setOdbcConnection <- function(channelName)
{
  if (missing(channelName))
  {
    tkmessageBox(message="setOdbcConnection: the channel name is missing!",icon="error",type="ok")
    return()
  }

  myConnection = odbcConnect(channelName)
	x <- odbcGetInfo(myConnection)

	if (x["DBMS_Name"] == "Microsoft SQL Server")
	{
    class(myConnection) = c(class(myConnection),"mssql")
    return(myConnection)
	}

	if (x["DBMS_Name"] == "MySQL")
	{
	  class(myConnection) = "mysql"
		tkmessageBox(message="determineDbType: MySQL is not supported!",icon="error",type="ok")
		return(1)
	}

	tkmessageBox(message="determineDbType: The database in use is not supported!",icon="error",type="ok")
	return(1)
}

rename.data.frame <- function(dataFrame, newRowNames, newColumnNames)
{
  if (!missing(newColumnNames))
  {
    ncols = dim(dataFrame)[[2]]
    columnNames = rep(newColumnNames, length=ncols)
    dimnames(dataFrame)[[2]] <- columnNames
	}

  if (!missing(newRowNames))
  {
    nrows = dim(dataFrame)[[1]]
    rowNames = rep(newRowNames, length=nrows)
    dimnames(dataFrame)[[1]] <- rowNames
	}
  return(dataFrame)
}

sql.get.table <- function (myConnection, tableName, query, fieldNames="*", as.is=TRUE)
{
  ## as.is = TRUE returns all fields as character (default in this implementation,
  ## but not the default in sqlQuery wich is as.is=FALSE).
  ## Example: as.is = c(1,2,3) returns the first 3 column as character, while the
  ## other ore converted according to the rule as in the man page sqlQuery:
  ## Where possible sqlGetResults transfers data directly: this happens for double,
  ## real, integer and smallint columns in the table. All other SQL data types are
  ## converted to character strings by the ODBC interface. If the as.is is true for
  ## a column, it is returned as character. Otherwise (where detected) date, datetime
  ## and timestamp values are converted to "Date" and "POSIXct" values respectively.

  ## colClasses can be one of the atomic vector classes 
  ## (logical, integer, numeric, complex, character, raw), or "factor", "Date" or "POSIXct".

	## 1. check for missing mandatory arguments
	if (missing(myConnection))
	{
		tkmessageBox(message="get.table: connection is missing!",icon="error",type="ok")
		return(NULL)
	}

  if (missing(tableName) & missing(query))
  {
    tkmessageBox(message="sql.get.table: tableName and query are both missing!",icon="error",type="ok")
		return(NULL)
  }


  ##2. if the query argument is missing construct the query which select the fieldNames from tableName
  if (missing(query))
  {
    query <- set.query(tableName, fieldNames)
  }

  ## 3. get the data
  result <- sqlQuery(myConnection, query, as.is=as.is)

  sqlGetErrMsg(channel=myConnection) 
  return(result)
}


# questa funzione prende i dati e crea un dataframe idoneo per creare una
# tablelist tenendo conto di eventuali esigenze di aggiornamento
tcl.get.table <- function(odbcConnection,tableName,query,allFields=F,
                          requiredFields,orderBy,updateDb=F,pkFieldNames=NULL,
                          odbcFieldTypes)
{
  missingQuery <- missing(query)
  missingRequiredFields <- missing(requiredFields)
  
  if (!missingQuery & allFields)
  {
    tkmessageBox(message="tcl.get.table: query with allFields=TRUE not possible!",
                 icon="error",type="ok")
    return() 
  }
  
  if (missingQuery & !allFields & missingRequiredFields)
  {
    tkmessageBox(message="tcl.get.table: query and requiredFields are missing, allFields=F!",
                 icon="error",type="ok")
    return()
  }
  
  # if query exists execute it and determine the fields by means of the
  # column names of the data.frame
  if (!missingQuery) 
  {
    dataFrame <- sql.get.table(odbcConnection,query=query)
  }
  
  if (allFields)
  {
    # query <- paste("sp_columns '",tableName,"'")
    query <- paste("SELECT * FROM",tableName)
    if (!missing(orderBy))
      {
        query <- paste(query,orderBy)
      }
    dataFrame <- sql.get.table(odbcConnection,query=query)
  }

  if (!missingRequiredFields & !allFields & missingQuery) # select the required Fields
  {
    tmp <- unlist(requiredFields,use.names=F)
    query <- paste(tmp,collapse="],[")
    query <- paste("SELECT [",query,"] FROM ",tableName,sep="")
    if (!missing(orderBy))
      {
        query <- paste(query,orderBy)
      }
    dataFrame <- sql.get.table(odbcConnection,query=query)
    rm(query,tmp)
  }
  
  
  tmp <- dimnames(dataFrame)
  fieldNames <- tmp[[2]]
  nbFields <- length(fieldNames)
  nbRows <- length(tmp[[1]])
  
  # if !missingRequiredFields & !missingQuery check that all requiredFields
  # have been selected
  if (!missingRequiredFields & !missingQuery)
  {
    tmp <- unlist(requiredFields,use.names=F)
    if (any(!is.element(tmp,fieldNames)))
    {
      tkmessageBox(message="tcl.get.table: requiredFields not in the query! Modify the query.",
                 icon="error",type="ok")
    return()
    }
  }

  if (nbFields>0)
  {
    # if requiredFields is missing create it as a list(tclName1=odbcName1,...)
    if (missingRequiredFields)
    {
      requiredFields <- fieldNames
      requiredColumns <- fieldNames
    } else {
      # if requiredFields is not missing, it is a list of the form columnname=fieldname.
      # however requiredfields may not contain all "columnname=fieldname"
      # pairs because when specifying the list "columname=" is not mandatory.
      # in this case extend the list with
      # the missing columnnames using as columnname the corresponding fieldname.

      # 1) complete the names of requiredFields
      tmpNames <- names(requiredFields)
      if (is.null(tmpNames))
      {
        requiredFields  <- fieldNames
        requiredColumns <- fieldNames
      } else {
        tmpValues <- unlist(requiredFields,use.names=F)
        is.missing <- tmpNames == ""
        tmpNames[is.missing] <- tmpValues[is.missing]

        requiredFields  <- fieldNames
        requiredColumns <- fieldNames
        names(requiredColumns) <- fieldNames
        requiredColumns[tmpValues] <- tmpNames
        names(requiredColumns) <- NULL
      }
    }
  }
  else
  {
    tkmessageBox(message="tcl.get.table: the table has no fields!",icon="error",type="ok")
    return()
  }

  columnNames <- requiredColumns        # will contain all the columnnames
  
  # rename the columns accordingly
  dimnames(dataFrame)[[2]] <- requiredColumns
  
  # check the existence of the primary keys
  if (updateDb)
  {
    if (missing(pkFieldNames))
    {
      query = paste("sp_pkeys '",tableName,"'",sep="")
      pkDataFrame <- sql.get.table(odbcConnection, query=query)

      nbPk <- dim(pkDataFrame)[[1]]
      if (nbPk>0)
      {
        pkFieldNames <- pkDataFrame[,"COLUMN_NAME"]
      }
      else
      {
        tkmessageBox(message=paste("tcl.get.table: the table",tableName,"is not editable!"),icon="error",type="ok")
        return()
      }
    } else {   # pkFieldNames is not missing
      nbPk <- length(pkFieldNames)
    }
    
    # determine the primary key fields still missing
    missingFields <- setdiff(pkFieldNames,fieldNames)
    if (length(missingFields)>0)
    {
      # get the missing part of the dataFrame
      query = paste(missingFields,collapse="],[")
      query <- paste("SELECT [",query,"] FROM ",tableName,sep="")
      if (!missing(orderBy))
        {
          query <- paste(query,orderBy)
        }
      pkData <- sql.get.table(odbcConnection,query=query)
      if (dim(pkData)[[1]]!=dim(dataFrame)[[1]])
      {
        tkmessageBox(message="tcl.get.table: the pkData and dataFrame have different number of rows!",
                     icon="error",type="ok")
        return()
      }
      # merge the dataFrame with the pkData
      for (i in 1:length(missingFields))
      {
        dataFrame[[missingFields[i]]] <- pkData[[missingFields[i]]]
      }
      
      # complete the fieldNames vector with the new names
      fieldNames <- c(fieldNames,missingFields)
      # complete the columnNames vector with the new names
      columnNames <- c(columnNames,missingFields)
    }
  } # end updateDb
  
  # construct the vector used to switch from columnNames to fieldNames
  columnToFieldNames <- fieldNames
  names(columnToFieldNames) <- columnNames
  
  fieldToColumnNames <- columnNames
  names(fieldToColumnNames) <- fieldNames

  # set the row names of the data frame as "1", ..., "n"
  if (nbRows > 0) setRowNamesDataFrame("dataFrame")
  if (missing(odbcFieldTypes))
  {
    odbcTypes <- sqlColumnsTypesInfo(odbcConnection,tableName,fieldNames)
  } else {
    # check that all fieldNames have a corresponding value in odbcFieldTypes
    tmpNames <- names(unlist(odbcFieldTypes))
    if (length(setdiff(fieldNames,tmpNames))>0)
      {
        tkmessageBox(message="Some ODBC fieldnames are missing in odbcFieldTypes!\nCaused by Primary Keys?",
                     icon="error",type="ok")
        return()
      }    
    odbcTypes <- unlist(odbcFieldTypes[fieldNames])
    class(odbcTypes) <- class(odbcConnection)[2]
  }
  names(odbcTypes) <- fieldNames
  rTypes <- sqlTypeConversion(odbcTypes)
  names(rTypes) <- fieldNames
  tclTypes <- sqlToTclType(odbcTypes)
  names(tclTypes) <- fieldNames
  
  # convert dates from odbc to the self defined string format
  isDate <- odbcTypes == "datetime"
  isCharacter <- rTypes == "character"
  for (i in 1:length(odbcTypes))
  {
    if (isDate[i]) dataFrame[[i]] <- as.tclDate(dataFrame[[i]])
    # convert all elements of data to characters
    if (isCharacter[i]) dataFrame[[i]] <- as.character(dataFrame[[i]])
  }


  return(list(dataFrame=dataFrame,fieldNames=fieldNames,columnNames=columnNames,
              pkFieldNames=pkFieldNames,requiredFields=requiredFields,requiredColumns=requiredColumns,
              columnToFieldNames=columnToFieldNames,fieldToColumnNames=fieldToColumnNames,
              odbcTypes=odbcTypes,rTypes=rTypes,tclTypes=tclTypes,
              odbcConnection=odbcConnection,tableName=tableName
             )
        )
} ## end tcl.get.table



sqlCommand <- function(channel,query,errorText="")
{
  result <- odbcQuery(channel, query)
  if (result != 1)
  {
    odbcErrMsg <- odbcGetErrMsg(channel)
    odbcClearError(channel)
    if(missing(errorText)) {msg <- odbcErrMsg} else {msg <- paste(errorText,"\n",odbcErrMsg,sep="")}
    tkmessageBox(message=msg,icon="error",type="ok")
  }
  return(result)
}


# Questa subroutine crea la struttura di una tabella di varianze - covarianze
# nel db specificato
create.cov.table <- function (table,channel,errorText=paste("create.cov.table(",table,")")) {

   query <- paste("CREATE TABLE ", table, " (Ticker1 int NOT NULL, ",
                  "Ticker2 int NOT NULL, Covarianza float NOT NULL",
                  "CONSTRAINT key_", table, " PRIMARY KEY (Ticker1,Ticker2))", sep = ""
                 )

   result <- sqlCommand(channel,query,errorText)
   return (invisible(result))
}



# Questa subroutine rimuove la tabella specificata del db specificato
drop.table <- function (table,channel,errorText=paste("drop.table(",table,")")) {
  query  <- paste("DROP TABLE ", table, sep="")
  result <- sqlCommand(channel,query,errorText)
  return (invisible(result))
}




# this function delete the "table" from a database
delete.table <- function (table,channel,errorText=paste("delete.table(",table,")")) {
  query  <- paste("DELETE FROM ", table, sep="")
  result <- sqlCommand(channel,query,errorText)
  return (invisible(result))
}

# this function return 
select.result <- function(channel,query,rows=1,cols=1)
  {
    result <- sql.get.table(myConnection=channel,query=query)
    return(result[rows,cols])
  }

# this function return the number of row from a "SELECT COUNT(*) ..." query
select.count <- function(channel,query)
  {
    result <- sql.get.table(myConnection=channel,query=query)
    return(result[1,1])
  }
  
# this function returns some of the sql types used when defining a field in a table
tableFieldTypes <- function(myConnection) UseMethod("tableFieldTypes")

tableFieldTypes.default <- function(myConnection)
{
  tkmessageBox(message="tableFieldTypes: invalid class for connection!",icon="error",type="ok")
  return(NULL)
}

tableFieldTypes.mssql <- function(myConnection)
{
  types <- c("bit","char","datetime","float","int",
             "nchar","ntext","nvarchar","real","text","varchar")
  class(types) <- "mssql"
	return(types)
}

fieldLengthString.mssql <- function(types)
  {
    x <- matrix(rep(c("","(",")",""),12),ncol=4,byrow=TRUE)
    columnNames <- c("type","left bracket","right bracket","identity")
    colnames(x) <- columnNames
    
    rownames(x) = c("bit","char","datetime","float","int","int identity",
             "nchar","ntext","nvarchar","real","text","varchar")
    x[,"type"] = rownames(x)
    
    ## Modifiy the fields that do not require the "()" 
    x[c("bit","int","int identity","datetime","float","real"),2:3] <- ""
    
    ## correct the "int identity" field
    x["int identity","identity"] <- "IDENTITY"     
    x["int identity","type"] = "int"  

    return(x[types,,drop=FALSE])

  }
  
## this is the function used to create a new table in a database
## df.tableStructure is a dataframe containing the following fields:
## data.frame(character(0),character(0),character(0),logical(0),logical(0),stringsAsFactors=FALSE)
## c("Column Name","Data Type","Length","Allow Nulls","Primary Key")
sqlCreateTable <- function(myConnection,database,newTable,df.tableStructure,exec=TRUE) UseMethod("sqlCreateTable")

sqlCreateTable.mssql <- function(myConnection,database,newTable,df.tableStructure,exec=TRUE)
  {
    if (nrow(df.tableStructure)==0) return(list(query1="",query2=""))
  
    ## get the initial database
    initialDb <- sqlWorkingDatabase(myConnection)

    ## remove square brackets
    database <- gsub("\\[|\\]","",database)
    
    ## change database if required
    if (initialDb != database) sqlQuery(channel=myConnection,query=paste("USE [",database,"]",sep=""))

    ## construct the query to define the fields
    query <- paste("CREATE TABLE [",newTable,"] (", sep="")
    tmp <- fieldLengthString.mssql(df.tableStructure[,"Data Type"])
    

    ## remove the Length for datatypes bit, int, ...                             
    delete <- is.element(df.tableStructure[,"Data Type"],
                         c("bit","int","int identity","datetime","float","real"))      
      
    df.tableStructure[delete,"Length"] <- ""  

    tmp1 <- paste(tmp[,"type"],tmp[,"left bracket"],df.tableStructure[,"Length"],
                  tmp[,"right bracket"]," ",tmp[,"identity"],sep="")
    tmp1 <- paste("[",df.tableStructure[,"Column Name"],"] ",tmp1,sep="") 
    nullNotNull <- c("NULL","NOT NULL");names(nullNotNull) <- c("1","0")
    nullNotNull <-  nullNotNull[as.character(as.numeric(df.tableStructure[,"Allow Nulls"]))]
    tmp1 <- paste(tmp1,nullNotNull,sep=" ",collapse=",")
    query <- paste(query,tmp1,")",sep="")
    rm(nullNotNull,tmp,tmp1)

    query1 <- query
        
    if (exec)
      {
        error <- sqlQuery(channel=myConnection,query=query,errors=FALSE)
        if (error == -1) sqlGetErrMsg(channel=myConnection) 
      }

    ## assign the primary keys if required
    isRequired <- df.tableStructure[,"Primary Key"] == TRUE
    if (any(isRequired))
      {
        tmp <- paste("[",df.tableStructure[isRequired,"Column Name"],"]",sep="",collapse=",")
        query <- paste("ALTER TABLE [",newTable,"] ADD CONSTRAINT [PK_",newTable,"] ",
                   "PRIMARY KEY (",tmp,")",sep="")
        
        if (exec)
          {
            error <- sqlQuery(channel=myConnection,query=query,errors=FALSE)
            if (error == -1) sqlGetErrMsg(channel=myConnection) 
          }
        query2 <- query
      }
    else
      {
        query2 <- ""
      }
      
    if (initialDb != database) 
      {
        error <- sqlQuery(channel=myConnection,query=paste("USE [",initialDb,"]",sep=""),errors=FALSE)
        if (error == -1) sqlGetErrMsg(channel=myConnection) 
      }
    return(list(query1=query1,query2=query2)) 
  }

  
sqlCreateTable.default <- function(myConnection,database,newTable,df.tableStructure)
{
  tkmessageBox(message="sqlCreateTable: invalid class for odbcCon!\nMethod for this class not implemented.",icon="error",type="ok")
  return()
}

