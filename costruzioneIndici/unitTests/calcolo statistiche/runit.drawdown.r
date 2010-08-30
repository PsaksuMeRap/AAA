test.calcoloDrawDownUp <- function() {
  x <- 1:100

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=15)  # caso periodo più lungo delle osservazioni
  s <- drawDown(x,T=0)   # caso per zero osservazioni

  checkEquals(y,0)
  checkEquals(z,0)
  checkEquals(v,0)
  checkEquals(w,0)
  checkEquals(s,NA_real_) 
}

test.calcoloDrawDownOneObs <- function() {
  x <- 1

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=15)  # caso periodo più lungo delle osservazioni

  checkEquals(y,0)
  checkEquals(z,0)
  checkEquals(v,0)
  checkEquals(w,0) 
}

test.calcoloDrawDownOneNA <- function() {
  x <- NA_real_

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni

  checkEquals(y,NA_real_)
  checkEquals(z,NA_real_)
  checkEquals(v,NA_real_)
}

test.calcoloDrawDownTwoNA <- function() {
  x <- rep(NA_real_,5)

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=15)  # caso periodo più lungo delle osservazioni

  checkEquals(y,NA_real_)
  checkEquals(z,NA_real_)
  checkEquals(v,NA_real_)
  checkEquals(w,NA_real_)
}

test.calcoloDrawDownThreeObsTwoNA <- function() {
  x <- c(NA,3.4,NA)

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=15)  # caso periodo più lungo delle osservazioni

  checkEquals(y,0)
  checkEquals(z,NA_real_)
  checkEquals(v,0)
  checkEquals(w,0)
}

test.calcoloDrawDownFourObsTwoNA <- function() {
  x <- c(NA,3.4,NA,0)

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=15)  # caso periodo più lungo delle osservazioni

  checkEquals(y,3.4)
  checkEquals(z,NA_real_)
  checkEquals(v,0)
  checkEquals(w,3.4)
}

test.calcoloDrawDown <- function() {
  x <- seq(10,1,by=-0.5)

  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=3)   # caso tre osservazioni
  s <- drawDown(x,T=30)  # caso periodo più lungo delle osservazioni

  checkEquals(y,9)
  checkEquals(z,0)
  checkEquals(v,0.5)
  checkEquals(w,1)  
  checkEquals(s,9) 
}


 test.calcoloDrawDownConNA <- function() {
  x <- seq(10,1,by=-0.5)
  x[1] <- NA_real_
  x[10] <- NA_real_
  x[length(x)] <- NA_real_
  
  y <- drawDown(x)       # caso standard
  z <- drawDown(x,T=1)   # caso una sola osservazione
  v <- drawDown(x,T=2)   # caso due osservazioni
  w <- drawDown(x,T=3)   # caso tre osservazioni
  s <- drawDown(x,T=30)  # caso periodo più lungo delle osservazioni

  checkEquals(y,9.5-1.5)
  checkEquals(z,NA_real_)
  checkEquals(v,0)
  checkEquals(w,9.5-9)  
  checkEquals(s,9.5-1.5) 
}