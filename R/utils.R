
modulo <- function( a, n ) { # returns a mod n 
    res <- a - floor(a/n)*n
    res
}

weight <- function( x ) {
    x <- as.vector(x)
    res <- x
    for( i in 1:length(res) ) {
        res[i] <- 0
        while( x[i] > 0 ) {
            res[i] <- res[i] + modulo(x[i],2)
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
    res <- vector("integer", n)
    for( i in 1:n ) {
        res[i] <- modulo(a,2)
        a <- floor(a/2)
    }
    res
}

