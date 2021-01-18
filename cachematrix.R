## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function, the created a special "matrix" object will cache its invers
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             ## holds the value of "matrix" inverse
        set <- function(y) {                    ## defines the set function - new
                x <<- y
                inv <<- NULL
        }
        get <- function() x                     ## defines the get function - return value
        setInverse <- function(inverse) inv <<- inverse ## assigns value of inverse
        getInverse <- function() inv                    ## gets value of inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
