## Two functions used for caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## sets matrix (i = inverse property; m = matrix)
        set <- function( m ) {
            x <<- m
            i <<- NULL
    }
    ## gets the matrix
        get <- function() x
    ## set the inverse of the matrix
        setInverse <- function(inverse) i <<- inverse
    ## get the inverse of the matrix
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## retrieves the inverse if already calculated
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
    ## returns already set inerse
        if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ## gets matrix
        data <- x$get()
    ## inverse matrix calculation
        m <- solve(data) %*% data
    ## sets inverse for our object
        x$setInverse(m)
    ## matrix is returned
        m
}
