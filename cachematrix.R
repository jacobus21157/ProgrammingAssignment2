## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set the inverse to NULL
    m <- NULL
    
    ## Initialize set
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## method for get and return
    get <- function() x
    
    ## Set the Inverse
    setInverse <- function(inverse) m <<- inverse
    
    ## Get the inverse
    getInverse <- function() m
    
    ## generate the list of methods
    list(set = set, get=get,
         setInverse =setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## sets m to the inverse of x
    m <- x$getInverse()
    
    ## if already computed, get cached data
    if(!is.null(m)) {
        message("getting chached data")
        return(m)
    }
    
    ## Get the matrix
    data <-x$get()
    
    #calculate the inverse using solve
    m <- solve(data, ...)
    
    ## set the inverse
    x$setInverse(data, ...)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
