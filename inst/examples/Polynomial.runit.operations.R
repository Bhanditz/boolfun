filename <- paste(sep="", system.file("examples",package="boolfun", "auxiliary.R"))
source(filename) # 'imports' the following
# N is the number of 'trials'
# rnd.int(a,b)  returns a random integer in [a,b]
# rnd.vec(n)    returns a random vector in {0,1}^n
# rnd.fun(n)    returns a random function from {0,1}^n to {0,1}
# rnd.pol(n)    returns a random polynomial with n var
Polynomial.test.addition1 <- function() {
  for( i in 1:N ) {
    n <- rnd.int(0,8)
    a <- rnd.pol( n )
    b <- rnd.pol( n )
    checkEquals(a+b,b+a)
    checkEquals(a+a,b+b)
    checkEquals(a+b+a, b+a+a)
  }
}
Polynomial.test.multiplication1 <- function() {
  for( i in 1:N ) {
    n <- rnd.int(0,8)
    a <- rnd.pol( n )
    b <- rnd.pol( n )
    checkEquals(a*b,b*a)
    checkEquals(a*a,a)
    checkEquals(b*b,b)
    checkEquals(b*a*b, a*b*b)
  }
}
Polynomial.test.addition2 <- function() {
  for( i in 1:N ) {
    na <- rnd.int(0,8)
    nb <- rnd.int(0,8)
    a <- rnd.pol( na )
    b <- rnd.pol( nb )
    checkEquals(a+b,b+a)
    checkEquals(a+a,b+b)
    checkEquals(a+b+a, b+a+a)
  }
}
Polynomial.test.multiplication2 <- function() {
  for( i in 1:N ) {
    na <- rnd.int(0,8)
    nb <- rnd.int(0,8)
    a <- rnd.pol( na )
    b <- rnd.pol( nb )
    checkEquals(a*b,b*a)
    checkEquals(a*a,a)
    checkEquals(b*b,b)
    checkEquals(b*a*b, a*b*b)
  }
}
Polynomial.test.operations1 <- function() { 
  for( i in 1:N ) {
    n <- rnd.int(0,10)
    a <- rnd.pol( n )
    b <- rnd.pol( n )
    c <- rnd.pol( n )
    x <- a + b*( a*(b + a) ) + c
    y <- a + a*b*(b + a) + c
    z <- c + a
    checkEquals(x,y)
    checkEquals(y,z)    
  }
}
Polynomial.test.operations2 <- function() { 
  a <- Polynomial("1110101010110101")
  b <- Polynomial("0101110101101001")
  c <- Polynomial("1110101010100001")
  expected <- "x1*x2*x4 + x1*x3*x4"
  for( i in 1:N ) {
    observed <- string( a + b*( a*(b + a) ) + c )
    checkEquals(observed,expected)
  }
}
Polynomial.test.operations3 <- function() {
  # test particular cases, like constant polynomials, ...
    checkEquals(1,1)
}

