test.annualizedMean <- function() {
  
  x <- numeric()  # caso 0 osservazioni
  y <- annualizedMean(x,freq="Daily")
  checkEquals(y,NA_real_)

  x <- 1.5  # caso 1 sola osservazione 
  y <- annualizedMean(x,freq="Daily")
  checkEquals(y,1.5*252)

  x <- NA_real_  # caso 1 osservazione NA 
  y <- annualizedMean(x,freq="Daily")
  checkEquals(y,NA_real_)

  x <- c(2.0,-3.0)  # caso 2 osservazioni 
  y <- annualizedMean(x,freq="Monthly")
  checkEquals(y,-0.5*12)

  x <- c(2.0,-3.0,NA_real_)  # caso 3 osservazioni con 1 NA 
  y <- annualizedMean(x,freq="Monthly")
  checkEquals(y,-0.5*12)

}

test.annualizedStedv <- function() {
  
  x <- numeric()  # caso 0 osservazioni
  y <- annualizedStdev(x,freq="Daily")
  checkEquals(y,NA_real_)

  x <- 1.5  # caso 1 sola osservazione 
  y <- annualizedStdev(x,freq="Daily")
  checkEquals(y,NA_real_)

  x <- NA_real_  # caso 1 osservazione NA 
  y <- annualizedStdev(x,freq="Daily")
  checkEquals(y,NA_real_)

  x <- c(2.0,-3.0)  # caso 2 osservazioni 
  y <- annualizedStdev(x,freq="Monthly")
  checkEquals(y,sqrt(12.5)*sqrt(12))

  x <- c(2.0,-3.0,NA_real_)  # caso 3 osservazioni con 1 NA 
  y <- annualizedStdev(x,freq="Monthly")
  checkEquals(y,sqrt(12.5)*sqrt(12))

  x <- c(2.0,-3.0,NA_real_,11.2)  # caso 4 osservazioni con 1 NA 
  y <- annualizedStdev(x,freq="Monthly")
  checkEquals(y,sqrt(51.88)*sqrt(12))

}