
mobiusTransform <- function ( truthTable ) # /!\ should check truthTable values are in {0,1}
{
    len <- log(length(truthTable),base=2)
    if( len != round(len) )
        stop("bad truth table length")
    res <- .Call( "mobiusTransform",
        as.integer(truthTable),
        as.integer(len))
    res
}

