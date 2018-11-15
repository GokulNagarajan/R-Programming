## Put comments here that give an overall description of what your
## Cache Inverse of Matrix in R
## functions do
## R-Programming Week3 Assignment

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(inv = matrix()) {
        inverse <- NULL
        set <- function(mat) {
            inv <<- mat
            inverse <<- NULL
        }
        get <- function() inv
        setinverse <- function(inversematrix) inverse <<- inversematrix
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(matfun, ...) {
        ## Return a matrix that is the inverse of 'inv'
        inverse <- matfun$getinverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- matfun$get()
        inverse <- solve(data, ...)
        matfun$setinverse(inverse)
        inverse
}
