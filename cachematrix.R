## A pair of functions provided to implement caching inverse of a matrix:
##  makeCacheMatrix: for making a matrix object that can cache its inverse.
##  cacheSolve: for calculating the inverse.



## Creates a special "matrix" object, that can cache its inverse.
##
## Args:
##     x: The input matrix for which the inverse will be calculated.
##        An invertable matrix expected.
## 
## Returns: 
##     A list of functions for setting and getting the input matrix and 
##     its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMatrix <<- inverse
        getinverse <- function() invMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Computes inverse of a matrix. 
## If already calculated, retrives the matrix from the cache. 
##
## Args:
##     x: A special "matrix" returned by makeCacheMatrix.
##  ... : further arguments passed to the function solve.
##
## Returns:
##     An inverse of the matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
