filename <- paste(sep="", system.file("examples",package="boolfun", "auxiliary.R"))
source(filename) # 'imports' the following
# N is the number of 'trials'
# rnd.int(a,b)  returns a random integer in [a,b]
# rnd.vec(n)    returns a random vector in {0,1}^n
# rnd.fun(n)    returns a random function from {0,1}^n to {0,1}
# rnd.pol(n)    returns a random polynomial with n var
BooleanFunction.test.mobius1 <- function() {
  for( i in 1:N ) {
    n <- rnd.int(0,10)
    tta <- rnd.vec(2^n)
    ttb <- rnd.vec(2^n)
    anfa <- mobiusTransform(tta)
    anfb <- anf(BooleanFunction(ttb))
    checkEquals( tta, mobiusTransform(anfa) )
    checkEquals( ttb, mobiusTransform(anfb) )
   }
}
BooleanFunction.test.walsh1 <- function() {
  for( i in 1:N ) {
    n  <- rnd.int(0,10)
    wh <- wh(rnd.fun(n))
    expected <- 2^(2*n) # Parseval
    observed <- sum(wh * wh)
    checkEquals( expected, observed )
  }
}
BooleanFunction.test.deg <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.ai <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.ci <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.nl <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.res <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.isRes <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.isCi <- function() {
  checkTrue(TRUE)
}
BooleanFunction.test.isBal <- function() {
  checkTrue(TRUE)
}

