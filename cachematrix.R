## cachematrix.R - Provides a special vector object that returns the inverse
## of a matrix. The inverse is cached after the first computation. Use this 
## vector when using the inverse of a large matrix whose inverse calculation
## is computationally expensive. 
##
## Usage:
## x<-matrix(c(-1,0,0,-1),2,2)
## xm <- makeCacheMatrix(x)
## ixm <- cacheSolve(xm)
## ixm <- cacheSolve(xm) # cached matrix inverse returned

## makeCacheMatrix creates a special matrix object that can cache its inverse
##
## Argument:
## x - numeric invertible matrix 

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


## cacheSolve returns the inverse of the matrix specified by the vector
## returned by makeCacheMatrix.
##
## Argument:
## x - Object created using the cache matrix object (special vector)
##
## Value:
## Matrix that is the inverse of 'x'. Cached value returned if inverse has
## been previously computed.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
