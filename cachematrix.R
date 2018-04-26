## With the following two functions, it's possible to cache the inverse of a matrix, rather then computing it repeatedly, if it's needed more then once
## I have put comments in the body of the functions, describing every step

## makeCacheMatrix creates a special object (a list) from the matrix.
    ## This object caches the matrix and it's inverse

makeCacheMatrix <- function(matrix=matrix()) {
    inverse <- NULL
    # up until this point, we have created place for the matrix and it's inverse in this special environment
    set <- function(setter) {
        matrix <<- setter
        inverse <<- NULL
        # set is a function, that clears the cache: it sets the matrix to be the actual one, and clears any possible former matrix's cache from inverse
    }
    get <- function() matrix
    # as a function named get, the newly cached matrix can be extracted
    setinverse <- function(inv) inverse <<- inv
    # setinverse caches the inverse's value (the newly computed one)
    getinverse <- function() inverse
    # as a function named getinverse, the inverse can be extracted from the cache
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    # our matrix is transformed to this list of functions
}

## cacheSolve returns the inverse matrix of the matrix.
    ## If the inverse has already be computed, it is returned from the cache.
    ## If not, then it's computed.

cacheSolve <- function(matrix, ...) {
    # cachesolve operates with this list of functions we created with the former function
    inverse <- matrix$getinverse()
    # the cached inverse gets extracted from the cache to the variable inverse
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # if there really was a cached inverse (inverse is not null), we can just return it, without actually having to computing it again
    # if there wasn't a cached inverse, the cacheSolve function has to compute and set it
    data <- matrix$get()
    # it first gets the cached matrix
    inverse <- solve(data, ...)
    # computes the inverse of the matrix
    matrix$setinverse(inverse)
    # and using the setinverse function, sets the newly counted inverse in the cachematrix, so it's cached now
    inverse
    # and returns it
}

