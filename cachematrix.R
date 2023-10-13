## Put comments here that give an overall description of what your
## functions do

#

## Write a short comment describing this function
## We've included the functions to set and get the values of 
## the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_mat <<- inverse
        getInverse <- function() inv_mat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## What this function does is that it ensures that if
## we have already calculated inverse, then we 
## do not calculate it again and cache it. 
## If not calculated before, then we calculate the inverse and store it for displaying later on.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInverse()
        if (!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(mat, ...)
        x$setInverse(inv_mat)
        inv_mat
}
