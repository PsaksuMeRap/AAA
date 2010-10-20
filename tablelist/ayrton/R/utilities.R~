# Utilities functions

"%+%" <- function(string1="",string2="") paste(string1,string2,sep="")


getStorageMode <- function(x)
{
  if(is.list(x)) ## if x is a list or a data.frame
  {
    if (length(x) == 0) return(storage.mode(x))
    return(sapply(x,storage.mode))
  }
  if (is.vector(x))
  {
    return(storage.mode(x))
  }

  if (is.matrix(x))
  {
    return(storage.mode(x))
  }
}


getClass <- function(x)
{
  if(is.list(x)) ## if x is a list or a data.frame
  {
    if (length(x) == 0) return(class(x))
    return(sapply(x,class))
  }
  if (is.vector(x))
  {
    return(class(x))
  }

  if (is.matrix(x))
  {
    return(class(x))
  }

}


as.tclDate <- function(x)
{
    return(substr(x,1,10))
}


get.tclSortModes <- function(x)
{
  # x is a vector of tclTypes
  # available sortmodes are: "integer","real","ascii","dictionary","command"
  tclSortMode <- c("integer","command","ascii","ascii","integer")
  names(tclSortMode) <- c("integer","double","character","date","logical")
  return(tclSortMode[x])
}


setRowNamesDataFrame <- function(dataFrameName)
{
#  text <- paste("dimnames(",dataFrameName,")[[1]] <- paste('r',1:dim(",
#                 dataFrameName,")[[1]],sep=\"\")",sep="")
   text <- paste("dimnames(",dataFrameName,")[[1]] <- 1:(dim(",
                  dataFrameName,")[[1]])",sep="")
  command <- parse(text=text)
  eval(command,envir=parent.frame())
}


# this function returns dataFrame1 \ dataFrame2 and
# dataFrame2 interseption dataFrame1.
available.desired.dataFrames <- function(dataFrame1,dataFrame2,specialCharacter="\r")
  {
    ncols1 <- ncol(dataFrame1)
    nrows1 <- nrow(dataFrame1)

    ncols2 <- ncol(dataFrame2)

    if (ncols1 != ncols2) return(print("error: dataframe with different number of columns"))

    if (ncols2==1)
      {
        is.NA <- is.na(dataFrame2[,1])
        nrows2 <- length(dataFrame2[!is.NA,1])
      }
    else
      {
        nrows2 <- nrow(dataFrame2)
      }

    if (nrows1==0 & nrows2==0)
      {
        return(list(dataFrame1=dataFrame1,dataFrame2=dataFrame2))
      }

    if (nrows1==0)
      {
        return(list(dataFrame1=dataFrame1,dataFrame2=dataFrame2[-(1:nrows2),]))
      }

    if (nrows2==0)
      {
        return(list(dataFrame1=dataFrame1,dataFrame2=dataFrame2))
      }

    # first compute dataFrame2 interseption dataFrame1
    tmp1 <- as.character(dataFrame1[,1])
    tmp2 <- as.character(dataFrame2[,1])
    if (ncols1 > 1)
      {
        for (i in 2:ncols1)
          {
            tmp1 <- paste(tmp1,dataFrame1[,i],sep=specialCharacter)
            tmp2 <- paste(tmp2,dataFrame2[,i],sep=specialCharacter)
          }
      }
    intersection <- intersect(tmp2,tmp1)
    difference <- setdiff(tmp1,intersection)
    if (length(intersection)>0)
      {
        list.intersection <- strsplit(intersection,specialCharacter)
      }
    else
      {
        list.intersection <- list()
      }
    if (length(difference)>0)
      {
        list.difference <- strsplit(difference,specialCharacter)
      }
    else
      {
        list.difference <- list()
      }

    leng2 <- length(list.intersection)
    leng1 <- length(list.difference)
    dataFrame1 <- dataFrame1[-(1:nrows1),]
    if (leng1>0)
      {
        for (i in 1:leng1)
          {
            dataFrame1[i,] = list.difference[[i]]
          }
      }
    dataFrame2 <- dataFrame2[-(1:nrows2),]
    if (leng2>0)
          {
        for (i in 1:leng2)
          {
            dataFrame2[i,] = list.intersection[[i]]
          }
      }
    return(list(dataFrame1=dataFrame1,dataFrame2=dataFrame2))
  }

chop <- function(s,n=1)
  {
    # s: a string or string vector.
    # n: the number of character to. remove
    return(substr(s, 1, nchar(s)-n))
  }
