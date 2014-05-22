##############################################################################
## Title    : Unit tests for Caching inverse of a matrix
## Author   : Ambika
## Date     : May-2014
## Notes    : Different set of unit tests
##############################################################################

### removing all the variables in the environment/workspace ###
rm(list = ls())

### source the main R script ###
source("cachematrix.R")

### Test case - a general function ###
testCase <- function(test,str,pcnt,tcnt){
        #if ( identical(i1,i2) && identical(t1,t2) )  { rs = "PASS"; pcnt = pcnt + 1 }
        rs <- "FAIL"
        if ( test ) { rs = "PASS"; pcnt = pcnt + 1 }
        print(paste(tcnt,". ",str," : ", rs))
        tcnt = tcnt + 1
        cnt <- c(pcnt, tcnt)
}
######################################

###             BLOCK 1         ###
testData <- matrix(stats::rnorm(90000), nrow=300, ncol=300)
test <- makeCacheMatrix(testData)
system.time(t1 <- cacheSolve(test)) ## first call; w/o cache
system.time(t2 <- cacheSolve(test)) ## second call; with cache

t <- testCase(TRUE,'If Functions throw no error',1,1)
######################################

###             BLOCK 2         ###
n=4
testData <- matrix(rnorm(n^2), nrow=n)
test <- makeCacheMatrix(testData)
system.time(t1 <- cacheSolve(test)) ## first call; w/o cache
system.time(t2 <- cacheSolve(test)) ## second call; with cache
i1 <- diag(n)
i2 <- round(t1 %*% testData, 4)

t <- testCase(identical(i1,i2) && identical(t1,t2),'Identify matrix cross check',t[1],t[2])
######################################

###             BLOCK 3         ###
#install.packages("magic")
library(magic)

testData <- magic(5)
A <- makeCacheMatrix(testData)
A1 <- A$get()
A2 <- A$getsolve()
cacheSolve(A) ### first time; w/o cache
A3 <- A$getsolve()
A4 <- cacheSolve(A)

## set the cacheResolve result as testData
A$set(testData)
B2 <- A$getsolve()
B3 <- cacheSolve(A)

A$set(B3)
B4 <- A$getsolve()
B5 <- cacheSolve(A)

cacheSolve(A)

t <- testCase(identical(A1,testData),'makeCacheMatrix$get : getData and testData are identical',t[1],t[2])
t <- testCase(is.null(A2),'makeCacheMatrix$getsolve: NULL',t[1],t[2])
t <- testCase(length(testData) == length(A3),'makeCacheMatrix$getsolve: Matrix exists and length is of testData',t[1],t[2])
t <- testCase(identical(A4,A3),'makeCacheMatrix$getsolve: cacheSolve from cache result data is identical to getSolve',t[1],t[2])
t <- testCase(is.null(B2),'makeCacheMatrix$getsolve: reset and check NULL',t[1],t[2])
t <- testCase(is.null(B4),'makeCacheMatrix$getsolve: set the inverse as data and NULL check',t[1],t[2])
t <- testCase(all.equal(testData,B5),'cacheSolve: reset the data to inverse of testData and compare the cacheSolve of the inverse data and test Data for equality',t[1],t[2])
######################################

pcnt = t[1]
tcnt = t[2]
print("")
print(paste("Total test cases :",tcnt))
print(paste("Total PASS % : ",((pcnt/tcnt)*100),'%'),sep = "")
################################################################################################
