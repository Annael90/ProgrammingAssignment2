## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function created a special "matrix" object that can cache
##the inverse of the created matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" created above.
##This function should retrieve the inverse from the cache if
##the inverse has already been calculated (and the matrix has not changed).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$detInverse(inv)
        inv
}
