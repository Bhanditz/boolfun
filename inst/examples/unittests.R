library(RUnit)
library(boolfun)

bofuTestSuite <- defineTestSuite(name="BooleanFunction", 
    dirs=system.file("examples", package="boolfun"), 
    testFileRegexp = "BooleanFunction.runit.+\\.R",
    testFuncRegexp = "BooleanFunction.test.+")

polyTestSuite <- defineTestSuite(name="Polynomial",
    dirs=system.file("examples", package="boolfun"),
    testFileRegexp = "Polynomial.runit.+\\.R",
    testFuncRegexp = "Polynomial.test.+")

utilsTestSuite <- defineTestSuite(name="utils",
    dirs=system.file("examples", package="boolfun"),
    testFileRegexp = "utils.runit.+\\.R",
    testFuncRegexp = "utils.test.+")

utilsResult <- runTestSuite(utilsTestSuite)
polyResult  <- runTestSuite(polyTestSuite)
bofuResult  <- runTestSuite(bofuTestSuite)

printTextProtocol( utilsResult, showDetails = FALSE )
printTextProtocol( polyResult,  showDetails = FALSE )
printTextProtocol( bofuResult,  showDetails = FALSE )

