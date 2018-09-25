## 
# These functions return the inverse of a matrix either from a cached copy 
# or a new calculation.
##

## 
# This function creates a special "matrix" object that can cache its inverse. 
#
# @params x [matrix] Matrix to be inversed.
# @returns x [matrix] Returns original matrix.
# @returns m [matrix] Returns stored matrix.
# @example
# mat <- matrix(c(7,5,6,7,9,10,5,3,1),3,3)
# smat <- makeCacheMatrix(mat)
##

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
# 
# @param x [makeCacheMatrix] Object to be inversed
# @param ... {arg} Arguments for the apply() functiion
# @returns m {matrix} Inversed Matrix
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- apply(data,c(1,2),function(d)1/d)
    x$setinverse(m)
    m
}
