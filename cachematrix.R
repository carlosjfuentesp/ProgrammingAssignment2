## functions to create and retrieve a special matrix object
## (1) makeCacheMatrix creates a special matrix object that caches its inverse
## (2) cacheSolve returns the cached inverse of the special matrix object
##     or calculates it


## Creates an "special" matrix object
## that contains (1) a matrix and (2) its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## returns the cached inverse of a matrix (if the cache is not empty),
## or calculates the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
