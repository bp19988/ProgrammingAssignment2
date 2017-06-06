## The pair of functions create a matrix object to store its inverse in a cache
## in order to save processing power

## Creates the cache matrix to store the the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function()x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse=setInverse, getInverse=getInverse)     
}

## Computes the inverse of the matrix stored in the created matrix cache
## Checks if the inverse has already been calculated and returns the inverse of
## the matrix from the cache if so

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setInverse(inv)
        inv
}
