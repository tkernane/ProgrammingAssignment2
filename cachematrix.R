## This function is intended to compute the inverse of a square matrix and
## to store the result in the environment as a cached object

## The first function create a cached matrix with its inverse as a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y  ## super assignment <<- operator does the assignment in the
                    ## enclosing environment
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function retrieves the inverse of the matrix if it was previously stored
## in the environment by the makeCacheMatrix function or computes the inverse if it was not computed before

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
