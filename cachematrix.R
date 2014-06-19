## Functions that are used to create a special object that stores a numeric
## matrix and cache's its inverse.

## makeCacheMatrix creates a special a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    ## function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## function to get the value of the matrix
    get <- function() x
    
    ## function to set the value of the inverse
    setinverse <- function(inverse) inverseMatrix <<- inverse
    
    ## function to get the value of the inverse
    getinverse <- function() inverseMatrix
    
    list(set = set, get = get,
             setmean = setinverse,
             getmean = getinverse)
    }
}

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getinverse()
    ## If inverse is in cache then return it
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    ## If inverse is NOT in cache
    ## compute, store and return inverse
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setinverse(inverseMatrix)
    inverseMatrix
}
