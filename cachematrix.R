## makeCacheMatrix initiates a matrix x and a
## variable for its inverse. Then it generates
## a list of functions for setting and getting
## the matrix x, and for setting and getting 
## its inverse.

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


## cacheSolve determines the inverse of a matrix.
## The argument is a variable of type makeCacheMatrix.
## If already existing, it takes the inverse from
## cache. If not, it calculates the inverse, places
## it in cache and returns it.

cacheSolve <- function(x, ...) {
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
