## Overall description of what your
## functions do:
## 1. caching a matrix and it's inverse: methods for setting and getting both objects
## 2. calculate the inverse if not already done and setting the inverse "inverse-x".


## this function
## first: it assigns a matrix to x and sets the object "inverse" to NULL
## then: a method for getting the matrix, a method to set the inverse of the matrix and a method for getting the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse-x = matrix()
        inverse-x <- NULL
        set <- function(y) {
                x <<- y
                inverse-x <<-NULL
        }
        get <- function () x
        setinverse <- function(inverse) inverse-x <<- inverse
        getinverse <- function() inverse-x
        list(set = set, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse-x <- x$getinverse()
        if(!is.null(inverse-x)) {
                message("getting cached data")
                return(inverse-x)

        }
        matrix-x <- x$get()
        inverse-x <- solve(matrix-x)
        x$setinverse(inverse-x)
        inverse-x
}
