
setConstructorS3("BooleanFunction", function(initializer=c(0)) { # default initializer is the constant function 0 (n=0)
    # For now, only truth tables can be used as initializers.
    if( is.character(initializer) ) {
        initializer <- strsplit(initializer,split="")
        initializer <- as.integer(initializer[[1]])
    }
    len <- log(length(initializer),base=2)
    if( len != round(len) )
        stop("BooleanFunction : bad truth table length")
    # These are (all) the private fields:
    extend(Object(), "BooleanFunction",
        .WH = NULL,  # the representations (WH, ANF) are computed once only (on demand)
        .ANF = NULL, # the following properties will be stored/cached so they are computed once only:
        .TT = initializer, # truth table
        .ai = NULL,  # algebraic immunity
        .deg = NULL, # algegraic degree
        .ci = NULL  # nonlinearity
    )
})

setMethodS3("print", "BooleanFunction", function(x, ...) {
    x <- paste("Boolean function with", (log(length(x$.TT), base=2)), "variables.")
    NextMethod("print")
})

setMethodS3("equals", "BooleanFunction", function(this, obj, ...) {
  ( identical(data.class(this), data.class(obj)) &&
    identical(this$.n, obj$.n) &&
    identical(this$.TT , obj$.TT )    )
})

#___________________ R E P R E S E N T A T I O N S _________________________

setMethodS3("tt", "BooleanFunction", function(this, ...) {
    as.vector(this$.TT)
})

setMethodS3("wh", "BooleanFunction", function(this, ...) {
    if( is.null(this$.WH) )
        this$.WH <- walshTransform( this$tt() )
    as.vector(this$.WH)
})

setMethodS3("anf", "BooleanFunction", function(this, ...) {
    if( is.null(this$.ANF) )
        this$.ANF <- mobiusTransform( this$tt() )
    as.vector(this$.ANF)
})

setMethodS3("ANF", "BooleanFunction", function(this, ...) {
    res <- Polynomial(this$anf())
    res
})

#_______________________ P R O P E R T I E S _______________________________

setMethodS3("n", "BooleanFunction", function(this, ...) { 
    log( length(this$tt()), base=2 )
})

setMethodS3("deg", "BooleanFunction", function(this, ...) {
    if( is.null(this$.deg) ) {
    this$anf() # computes the anf if not done already
    this$.deg <- .Call( "degree",
        as.integer(this$anf()),
        as.integer(log(length(this$.ANF),base=2)))
    }
    as.integer(this$.deg)
})

setMethodS3("ai", "BooleanFunction", function(this, ...) {
    if( is.null(this$.ai) )
        this$.ai <- .Call( "algebraicImmunity",
            as.integer(this$tt()),
            as.integer(log(length(this$tt()),base=2)))
    as.integer(this$.ai)
})

setMethodS3("ci", "BooleanFunction", function(this, ...) {
    if( is.null(this$.ci) ) {
        this$.ci <- 0
        while( this$isCi( order=(this$.ci + 1) ) )
            this$.ci <- this$.ci + 1
    }
    as.integer(this$.ci)
})

setMethodS3("res", "BooleanFunction", function(this, ...) {
    res <- 0
    if( ! this$isBal() ) res <- -1
    else res <- this$ci()
    as.integer(res)
})

setMethodS3("nl", "BooleanFunction", function(this, ...) {
    WH <- this$wh()
    res <- ( length(WH) - max(abs(WH))  ) / 2
    as.integer(res)
})

setMethodS3("isBal", "BooleanFunction", function(this, ...) {
    res <- length(which(this$tt()==1))==length(which(this$tt()==0))
    as.logical(res)
})

setMethodS3("isCi", "BooleanFunction", function(this, order, ...) {
    res <- FALSE
    if( 0 < order && order < this$n() ) {
        WH <- this$wh()
        inputs <- which( weight(1:(length(WH)-1))==order )
        outputs <- WH[1+inputs] # /!\ R is one-based
        if( sum(outputs)==0 )
            res <- TRUE
    } else { 
        if(order==0) res <- this$isBal() 
    }
    as.logical(res)
})

setMethodS3("isRes", "BooleanFunction", function(this, order, ...) {
    res <- FALSE
    if( this$isBal() ) 
        res <- this$isCi(order)
    as.logical(res)
})

setMethodS3("isRes", "BooleanFunction", function(this, order, ...) {
    res <- FALSE
    if( this$isBal() ) 
        res <- this$isCi(order)
    as.logical(res)
})

