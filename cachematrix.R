## Functions for handling the caching of matrix inverse


## makeCacheMatrix
## Create a matrix with cachable inverse from the provided matrix

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve
## Cache and return the inverse of a cachable matrix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
