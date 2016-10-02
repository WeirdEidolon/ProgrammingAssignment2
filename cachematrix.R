## create an object that wraps a matrix and can cache its inverse. if the matrix
## changes, the cached value is cleared and will be recalculated later.

## create CacheMatrix object to store a mtrix and its inverse. calculating the inverse
## of a matrix can be a costly operation, caching in this way allows the calculation
## to be performed once and then looked up later.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inv) {
        i <<- inv
    }
    
    getinverse <- function() {
        i
    }
    
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## return the inverse of the matrix stored in a CacheMatrix object. if the inverse has 
## not been calculated do so and cache it on the CacheMatrix object. otherwise return 
## the cached inverse.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        # message("returning cached inverse")
        return(i)
    }
    
    # message("solving and caching inverse")
    i <- solve(x$get(), ...)
    x$setinverse(i)
    i
}
