test <- function(x)
  {
    nbStatistics <- length(x$statistics)
    allnames <- names(x$statistics)
    if ( nbStatistics == 11)
      {
        namesPart1 <- allnames[1:3]
        namesPart2 <- allnames[4:nbStatistics]
      }
    else
      {
        namesPart1 <- allnames[1]
        namesPart2 <- allnames[2:nbStatistics]
      }
    string1 <- paste(paste(allnames,collapse="\t"),"\n")
    string2 <- paste(as.integer(x$statistics[namesPart1]),collapse="\t")
    string3 <- paste(round(x$statistics[namesPart2], digits = 2),collapse="\t")
    string4 <- paste(string1,string2,"\t",string3)
    print(string4)
  }
  