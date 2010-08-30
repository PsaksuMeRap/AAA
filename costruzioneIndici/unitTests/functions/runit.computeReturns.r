

test.computePercReturns <- function() {
  
  ## costruisci data.frame dei prezzi
  prezzi <- data.frame(UBS=c(100.0,104.5,104),IBM=c(1,1,1),ROCHE=c(1.0,0.3,0.5))
  dimnames(prezzi) <- list(c("A-","AB","pippo e claudio"),c("UBS","IBM","ROCHE"))

  rendimenti <- computePercReturns(prezzi)
 
  checkEquals(ncol(rendimenti), 3)
  checkEquals(nrow(rendimenti),2)
  checkEquals(rendimenti[,"UBS"],c((104.5-100.0)/100.0,(104-104.5)/104.5))
  checkEquals(rendimenti[,"IBM"],c(0,0))
  checkEquals(rendimenti[,"ROCHE"],c((0.3-1.0)/1.0,(0.5-0.3)/0.3))
  checkEquals(is.data.frame(rendimenti), TRUE)
  checkEquals(dimnames(rendimenti),list(c("AB","pippo e claudio"),c("UBS","IBM","ROCHE")))
}

test.computePercReturnsWithZero <- function() {
  
  ## costruisci data.frame dei prezzi
  prezzi <- data.frame(UBS=c(0,104.5,104),IBM=c(1,1,1),ROCHE=c(1.0,NA_real_,0.5) )
  dimnames(prezzi) <- list(c("A-","AB","pippo e claudio"),c("UBS","IBM","ROCHE"))
  
  rendimenti <- computePercReturns(prezzi)

  checkEquals(is.data.frame(rendimenti), TRUE) 
  checkEquals(ncol(rendimenti), 3)
  checkEquals(nrow(rendimenti),2)
  checkEquals(rendimenti[,"UBS"],c(NA_real_,(104-104.5)/104.5))
  checkEquals(rendimenti[,"ROCHE"],c(NA_real_,NA_real_) )
  checkEquals(dimnames(rendimenti),list(c("AB","pippo e claudio"),c("UBS","IBM","ROCHE"))) 
}


test.computeLogReturns <- function() {
  
  ## costruisci data.frame dei prezzi
  prezzi <- data.frame(UBS=c(100.0,104.5,104),IBM=c(1,1,1),ROCHE=c(1.0,0.3,0.5))
  dimnames(prezzi) <- list(c("A-","AB","pippo e claudio"),c("UBS","IBM","ROCHE"))
  
  rendimenti <- computeLogReturns(prezzi)

  checkEquals(is.data.frame(rendimenti), TRUE) 
  checkEquals(ncol(rendimenti), 3)
  checkEquals(nrow(rendimenti),2)
  checkEquals(rendimenti[,"UBS"],c(log(104.5/100.0),log(104/104.5)))
  checkEquals(rendimenti[,"IBM"],c(0,0))
  checkEquals(rendimenti[,"ROCHE"],c(log(0.3/1.0),log(0.5/0.3)))
  checkEquals(dimnames(rendimenti),list(c("AB","pippo e claudio"),c("UBS","IBM","ROCHE"))) 
}

test.computeLogReturnsWithZero <- function() {
  
  ## costruisci data.frame dei prezzi
  prezzi <- data.frame(UBS=c(0,104.5,104),IBM=c(1,1,1),ROCHE=c(1.0,NA_real_,0.5) )
  dimnames(prezzi) <- list(c("A-","AB","pippo e claudio"),c("UBS","IBM","ROCHE"))
  
  rendimenti <- computeLogReturns(prezzi)

  checkEquals(is.data.frame(rendimenti), TRUE)
  checkEquals(ncol(rendimenti), 3)
  checkEquals(nrow(rendimenti),2)
  checkEquals(rendimenti[,"UBS"],c(NA_real_,log(104/104.5)))
  checkEquals(rendimenti[,"ROCHE"],c(NA_real_,NA_real_) ) 
  checkEquals(dimnames(rendimenti),list(c("AB","pippo e claudio"),c("UBS","IBM","ROCHE")))
}
