filename <- paste(sep="", system.file("examples",package="boolfun", "auxiliary.R"))
source(filename) # 'imports' the following
# N is the number of 'trials'
# rnd.int(a,b)  returns a random integer in [a,b]
# rnd.vec(n)    returns a random vector in {0,1}^n
# rnd.fun(n)    returns a random function from {0,1}^n to {0,1}
# rnd.pol(n)    returns a random polynomial with n var
utils.test.weight <- function() {
  w1 <- c(2,4,8,16,32,64,128,256,512,1024)
  w2 <- w1 + 1
  checkTrue( all(weight(w1)==1) )
  checkTrue( all(weight(w2)==2) )
  checkTrue( weight(15)==4 )
}
utils.test.toBin1 <- function() {
  for( i in 1:10 ) {
    checkEquals( toBin(i,0), c() )
    checkEquals( toBin(i,1), i%%2 )
  }
}
utils.test.toBin2 <- function() {
  checkEquals(1,1)
}
utils.test.toInt1 <- function() {
  checkEquals(1,1)
}
utils.test.toInt2 <- function() {
  checkEquals(1,1)
}
utils.test.toIntAndToBin <- function() {
  minBits <- 6
  maxBits <- 100
  for( i in 1:60 ) {
    n <- round(runif(1,minBits,maxBits))
    checkEquals( i, toInt(toBin(i,n)) )
  }
}
utils.test.strip <- function() {
  str1 <- strip("this is a first example", "i")
  str2 <- strip("this is a second example", c("i"," ","e"))
  str3 <- strip("this is the third example", c("w", "*"))
  str4 <- strip("this is the last example", c("", "s"))
  checkEquals(str1, "ths s a frst example")
  checkEquals(str2, "thssascondxampl")
  checkEquals(str3, "this is the third example")
  checkEquals(str4, "thi i the lat example")
}

