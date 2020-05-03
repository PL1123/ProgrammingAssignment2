## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function generates a "matrix" object (a list) that is 
## able to cache its inverse. It follows the same logic 
## as the example given. 

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


## Write a short comment describing this function
## Following the same logic as the example given, this function
## calculates the inverse of "matrix" (the list) returned by the previous 
## function. It can save computation time since the calculated  
## inverse will simply be retrieved from the cache rather than 
## being computed again. The new one will be computed. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


