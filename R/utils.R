weight <- function( x ) {
  x <- as.vector(x)
  res <- x
  for( i in 1:length(res) ) {
    res[i] <- 0
    while( x[i] > 0 ) {
      res[i] <- res[i] + (x[i]%%2)
      x[i] <- floor(x[i]/2)
    }
  }
  res
}
toBin <- function( a, n ) {
  a <- as.integer(a)
  n <- as.integer(n)
  if(length(a)!=1 || length(n)!=1)
    stop( "bad argument" )
  if( n==0 )
    res <- c()
  else {
    res <- vector("integer", n)
    for( i in 1:n ) {
    res[i] <- a %% 2
    a <- a %/% 2
    }
  }
  res
}
toInt <- function( v ) {
  res <- 0
  pow <- 1
  for( i in 1:length(v) ) {
    if( v[i]!=0 )
      res <- res + pow
    pow <- pow*2
  }
  res
}
strip <- function( string, chars ) {
  res <- strsplit( split="", string )[[1]]
  for( c in chars )
      res <- res[which(res!=c)]
  res <- paste(res, collapse="")
}

