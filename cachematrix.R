##########################################################################################
## Title    : Caching inverse of a matrix                                               ##
## Author   : Ambika                                                                    ##
## Date     : May-2014                                                                  ##
## Notes    :  caching the inverse of a matrix rather than computing it repeatedly      ##
##                We assume that the matrix supplied is always invertible               ##
## Other equivalent functions :                                                         ##
##            We can also use ginv() of library MASS pacakge (instead of solve()).      ##
##                                                                                      ##
## Example  : testData <- matrix(stats::rnorm(90000), nrow=300, ncol=300)               ##
##              test <- makeCacheMatrix(testData)                                       ##
##              system.time(t <- cacheSolve(test)) ## first call; w/o cache             ##
##              system.time(t <- cacheSolve(test)) ## second call; with cache           ##
##              sum(round(t %*% testData,4))       ## sum of the identity matrix; 300   ##
########################################################################################## 

## This function creates a special "matrix" object that can cache its inverse
## Parameters: n*n matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)   
}


## This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
##    then cacheinverse should retrieve the inverse from the cache.
## Parameters: object of "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m            
}

##########################################################################################