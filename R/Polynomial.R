
setConstructorS3("Polynomial", function(initializer=c()) { 
    # For now, only truth tables can be used as initializers.
    if( is.character(initializer) ) {
        initializer <- strsplit(initializer,split="")
        initializer <- as.integer(initializer[[1]])
    }
    n <- log(length(initializer),base=2)
    if( n != round(n) )
        stop("Polynomial : bad argument length")
    if( !all(initializer %in% c(0,1)) )
        stop("Polynomial : bad argument value")
    # These are (all) the private fields:
    extend(Object(), "Polynomial",
        .anf = initializer, # vector of coefficients
        .str = "", # polynomial as string
        .deg = NULL, # algebraic degree
        .n = n # number of variables
    )
})

setMethodS3("print", "Polynomial", function(x, ...) {
    x <- x$string()
    NextMethod("print")
})

setMethodS3("equals", "Polynomial", function(this, other, ...) {
  ( identical(data.class(this), data.class(other)) &&
    identical(this$.anf, other$.anf)    )
})

#____________________________________________________________________________________

setMethodS3("n", "Polynomial", function(this, ...) {
    this$.n
})

setMethodS3("anf", "Polynomial", function(this, ...) {
    this$.anf
})

setMethodS3("len", "Polynomial", function(this, ...) {
    length(this$anf())
})

setMethodS3("deg", "Polynomial", function(this, ...) {
    if( is.null(this$.deg) ) {
      this$.deg <- .Call( "degree",
          as.integer(this$anf()),
          as.integer(this$n()))
    }
    this$.deg
})

setMethodS3("string", "Polynomial", function(this, sort=TRUE, ...) {
    if( equals(this$.str,"") ) {
      idx <- which(this$anf()==1) 
      last <- idx[length(idx)]
      for( i in idx ) {
        plus <- if( i!=last ) " + " else ""
        monomial <- string(
           Assignment(toBin(i-1,this$n())))
        this$.str <- paste(this$.str,monomial,plus,sep="")
      }
      if( equals(this$.str,"") )
      this$.str <- "0"
    }
    if( sort ) {
      this$.str <- strsplit( this$.str, split="\ \\+\ " )[[1]]
      if( length(this$.str) > 1 ) {
        N <- length(this$.str)
        degs <- vector("integer",N)
        for( i in 1:N ) 
          degs[i] = length(strsplit(this$.str[i],"\\*")[[1]])
        idx <- sort( degs, decreasing=FALSE, index.return=TRUE)$ix
        this$.str <- paste(this$.str[idx], collapse=" + ")
      }
    }  
    this$.str
})

#____________________________________________________________________________________
setMethodS3("pad", "Polynomial", function(this, n, ...) {
    if(this$n() < n) {
      padding <- vector("integer", 2^n - 2^(this$n()))
      this$.anf = c(this$.anf, padding)
      this$.n = n
    }
})
setMethodS3("*", "Polynomial", function(e1, e2, ...) {
    if( e1$n() < e2$n() )
        res <- e2 * e1
    else {
      if( e1$n() > e2$n() ) 
        e2$pad(e1$n())
      res <- .Call( "multiply", 
        as.integer( e1$anf()),
        as.integer(e2$anf()),
        as.integer(e1$n() ))
      res <- Polynomial(res)
    }
    res
})

setMethodS3("+", "Polynomial", function(e1, e2, ...) {
    if( e1$n() < e2$n() )
        res <- e2 + e1
    else {
      if( e1$n() > e2$n() ) 
        e2$pad(e1$n())
      res <- .Call( "add", 
        as.integer( e1$anf()),
        as.integer(e2$anf()),
        as.integer(e1$n() ))
      res <- Polynomial(res)
    }
    res
})

setMethodS3("[[", "Polynomial", function(this, x, ...) {
  if(length(x) != this$n())
    stop("bad argument length")
  if(this$n()==0) 
    res <- this$anf()[1]
  else {
    truthTable <- mobiusTransform(this$anf())
    res <- truthTable[1+toInt(x)] 
  }
  res   
})

