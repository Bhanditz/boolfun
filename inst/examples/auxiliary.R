
N <- 10 # adjust according to paranoia 

rnd.int <- function(a,b) { # returns a random integer in [a,b]
  res <- round(runif(1,a,b))
  res
}

rnd.vec <- function( n ) {
  res <- round(runif(n,0,1))
}

rnd.fun <- function( n ) { # returns a random BooleanFunction with n var
  res <- BooleanFunction(round(runif(2^n,0,1)))
  res
}

rnd.pol <- function( n ) { # returns a random Polynomial with n var
  res <- Polynomial(round(runif(2^n,0,1)))
  res
}

