
## makeCacheMatrix creates an object to be used to cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The function cacheSolve calculate the inverse of the matrix x applying 
##the solve function to the makeCacheMatrix object (or returns a cached value)

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)){
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(i)
        i
        }
