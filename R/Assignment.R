
setConstructorS3("Assignment", function(initializer=c()) { 
    # For now, only truth tables can be used as initializers.
    if( is.character(initializer) ) {
        initializer <- strsplit(initializer,split="")
        initializer <- as.integer(initializer[[1]])
    }
    # These are (all) the private fields:
    extend(Object(), "Assignment",
        .val = initializer, # vector of integers
        .len = length(initializer), 
        .str = "" # monomial as string
    )
})

#________________________ O V E R R I D D E N    I N H E R I T E D    M E T H O D S__________________________ 

setMethodS3("print", "Assignment", function(x, ...) {
    x <- paste("Assignment with", len(x), "variables.")
    NextMethod("print")
})

setMethodS3("equals", "Assignment", function(this, obj, ...) {
  ( identical(data.class(this), data.class(obj)) &&
    identical(this$.len, obj$.len) &&
    identical(this$.val , obj$.val )    )
})

#____________________________________________________________________________________________________________

setMethodS3("string", "Assignment", function(this, ...) {
    if( equals(this$.str,"") ) {
      idx <- which(this$.val==1)
      last <- idx[length(idx)]
      for( i in idx ) {
        star <- if( i!=last ) "*" else ""
        this$.str <- paste(this$.str, 
          "x", i, star, sep="")  }}
    if(equals(this$.str,""))
        this$.str <- "1"
    this$.str
})

setMethodS3("int", "Assignment", function(this, ...) {
    res <- toInt(this$.val)
    res
})

setMethodS3("len", "Assignment", function(this, ...) {
    this$.len
})

setMethodS3("weight", "Assignment", function(this, ...) {
    sum(this$.val)
})

