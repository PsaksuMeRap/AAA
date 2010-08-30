test.maxDrawdown <- function() {
  x <- c(1.5,1,2,0)

  y <- maxDrawDown(x)       # caso standard
  s <- maxDrawDown(x,T=0)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2)   # caso due osservazioni
  w <- maxDrawDown(x,T=15)  # caso periodo più lungo delle osservazioni


  checkEquals(y,2)
  checkEquals(s,NA_real_)
  checkEquals(z,0)
  checkEquals(v,0.5)
  checkEquals(w,2) 
}


test.maxDrawdownWithNA_AtEnd <- function() {
  x <- c(1.5,1,2,0,NA_real_)

  y <- maxDrawDown(x)       # caso standard
  s <- maxDrawDown(x,T=0)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2)   # caso due osservazioni
  w <- maxDrawDown(x,T=15)  # caso periodo più lungo delle osservazioni


  checkEquals(y,2)
  checkEquals(s,NA_real_)
  checkEquals(z,0)
  checkEquals(v,0.5)
  checkEquals(w,2) 
}

test.maxDrawdownWithNA_AtStart <- function() {
  x <- c(NA_real_,1.5,1,2,0)

  y <- maxDrawDown(x)       # caso standard
  s <- maxDrawDown(x,T=0)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2)   # caso due osservazioni
  w <- maxDrawDown(x,T=15)  # caso periodo più lungo delle osservazioni


  checkEquals(y,2)
  checkEquals(s,NA_real_)
  checkEquals(z,NA_real_)
  checkEquals(v,0)
  checkEquals(w,2) 
}

# test the % maxdrawdown
test.maxPercDrawdown <- function() {
  x <- c(1.5,1,2,0)

  y <- maxDrawDown(x,percentage=TRUE)       # caso standard
  s <- maxDrawDown(x,T=0,percentage=TRUE)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1,percentage=TRUE)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2,percentage=TRUE)   # caso due osservazioni
  w <- maxDrawDown(x,T=15,percentage=TRUE)  # caso periodo più lungo delle osservazioni


  checkEquals(y,100.0 *(2.0-0)/2.0)
  checkEquals(s,NA_real_)
  checkEquals(z,0)
  checkEquals(v,100*(1.5-1)/1.5)
  checkEquals(w,100.0 *(2.0-0)/2.0) 
}

test.maxPercDrawdownWithNA_AtEnd <- function() {
  x <- c(1.5,1,2,0,NA_real_)

  y <- maxDrawDown(x,percentage=TRUE)       # caso standard
  s <- maxDrawDown(x,T=0,percentage=TRUE)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1,percentage=TRUE)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2,percentage=TRUE)   # caso due osservazioni
  w <- maxDrawDown(x,T=15,percentage=TRUE)  # caso periodo più lungo delle osservazioni

  checkEquals(y,100.0 *(2.0-0)/2.0)
  checkEquals(s,NA_real_)
  checkEquals(z,0)
  checkEquals(v,100*(1.5-1)/1.5)
  checkEquals(w,100.0 *(2.0-0)/2.0) 
}

test.maxDrawdownWithNA_AtStart <- function() {
  x <- c(NA_real_,1.5,1,2,0)

  y <- maxDrawDown(x,percentage=TRUE)       # caso standard
  s <- maxDrawDown(x,T=0,percentage=TRUE)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1,percentage=TRUE)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2,percentage=TRUE)   # caso due osservazioni
  w <- maxDrawDown(x,T=15,percentage=TRUE)  # caso periodo più lungo delle osservazioni

  checkEquals(y,100.0 *(2.0-0)/2.0)
  checkEquals(s,NA_real_)
  checkEquals(z,NA_real_)
  checkEquals(v,0)
  checkEquals(w,100.0 *(2.0-0)/2.0) 
}

test.maxPercDrawdownWithNA <- function() {
  x <- c(NA_real_,1.5,NA_real_,1,2,NA_real_,0)

  y <- maxDrawDown(x,percentage=TRUE)       # caso standard
  s <- maxDrawDown(x,T=0,percentage=TRUE)   # caso per zero osservazioni
  z <- maxDrawDown(x,T=1,percentage=TRUE)   # caso una sola osservazione
  v <- maxDrawDown(x,T=2,percentage=TRUE)   # caso due osservazioni
  p <- maxDrawDown(x,T=3,percentage=TRUE)   # caso tre osservazioni
  q <- maxDrawDown(x,T=4,percentage=TRUE)   # caso quattro osservazioni
  w <- maxDrawDown(x,T=15,percentage=TRUE)  # caso periodo più lungo delle osservazioni


  checkEquals(y,100.0 *(2.0-0)/2.0)
  checkEquals(s,NA_real_)
  checkEquals(z,NA_real_)
  checkEquals(v,0)
  checkEquals(p,0)
  checkEquals(q,100*(1.5-1.0)/1.5)
  checkEquals(w,100.0 *(2.0-0)/2.0) 
}




