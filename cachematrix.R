## Put comments here that give an overall description of what your
## Cache Inverse of Matrix in R
## functions do
## R-Programming Week3 Assignment

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(inv = matrix()) {
		inverseMatrix <- NULL
        set <- function(mat) {
                inv <<- mat
                inverseMatrix <<- NULL
        }
        get <- function() inv
        setInverseMatrix	 <- function(inv) inverseMatrix <<- inv
        getInverseMatrix <- function() inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
		inv <- mat$getInverseMatrix
        if(!is.null(inv)) {
                message("getting cached data")
				return(inv)
        }
        data <- mat$get()
        inv <- solve(data, ...)
        mat$setInverseMatrix(inv)
		inv
}
