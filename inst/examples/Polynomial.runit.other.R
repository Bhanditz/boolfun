filename <- paste(sep="", system.file("examples",package="boolfun", "auxiliary.R"))
source(filename) # 'imports' the following
# N is the number of 'trials'
# rnd.int(a,b)  returns a random integer in [a,b]
# rnd.vec(n)    returns a random vector in {0,1}^n
# rnd.fun(n)    returns a random function from {0,1}^n to {0,1}
# rnd.pol(n)    returns a random polynomial with n var
Polynomial.test.string1 <- function() {
  for( i in 1:N ) {
    n <- rnd.int(0,10)
    a <- Polynomial(vector("integer",2^n))
    checkEquals(string(a),"0")
  }
  checkEquals(string(Polynomial("0101")), "x1 + x1*x2")
}
Polynomial.test.value1 <- function() { # tests operator [[
  for( i in 1:N ) {
    n <- rnd.int(0,6)
    zeros <- vector("integer",n)
    p <- rnd.pol(n)
    checkEquals(p$anf()[1], p[[zeros]])
  }
}
Polynomial.test.value2 <- function() { # tests operator [[, and more.
  for( i in 1:N ) {
    n <- rnd.int(0,6)
    p <- rnd.pol(n)
    truthTable <- mobiusTransform(anf(p))
    for( i in 0:(n-1) ) if( i>-1) {
      vec <- toBin( i,n )
      checkEquals( truthTable[1+i] , p[[vec]] )
    }
  }
}
Polynomial.test.len <- function() {
  for( i in 1:N ) {
    n <- rnd.int(0,20)
    p <- rnd.pol( n )
    expected <- 2^n
    observed <- len(p)
    checkEquals(observed, expected)
  }
}
Polynomial.test.deg <- function() {
  for( i in 1:N ) {
    n <- rnd.int(0,7)
    p <- Polynomial(vector("integer",2^n))
    q <- Polynomial(c( vector("integer",2^n-1), 1))
    checkEquals(deg(p),0)
    checkEquals(deg(q),n)
  }
}
Polynomial.test.initializer1 <- function() {
  expected1 <- c(0,1,0,1,1,0,0,1,1,0,0,1,0,0,1,1)
  expected2 <- "1 + x1 + x1*x4 + x1*x2*x3"
  p1 <- Polynomial("0101100110010011")
  p2 <- Polynomial("1+ x1 +x1 x4 + x1* x2 *x3 ")
  checkEquals(anf(p1), expected1)
  checkEquals(string(p2), expected2)
}
Polynomial.test.initializer2 <- function() {
  for( i in 1:N ) {
    f <- rnd.fun(rnd.int(2,8))
    expected <- anf(f)
    observed <- anf(Polynomial(string(f$ANF())))
    idx <- 1:max(length(expected),length(observed))
    checkEquals(expected[idx], observed[idx]) 
  }
}
Polynomial.test.initializer3 <- function() {
  expected0 <- c(0)
  expected1 <- c(1)
  expected2 <- c()
  p0 <- Polynomial("0")
  p1 <- Polynomial("1")
  checkEquals(anf(p0), expected0)
  checkEquals(anf(p1), expected1)
}
Polynomial.test.initializer4 <- function() {
  p1 <- Polynomial("")
  p2 <- Polynomial("0 + 1 + 0")
  p3 <- Polynomial("x3")
  checkEquals(1,1)
}
Polynomial.test.zero <- function() {
  # test zero polynomial
  checkEquals(1,1)
}

