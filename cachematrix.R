## This script contains two functions that enable to compute and cache 
## the inverse of a matrix, and to retrieve a previously computed inverse matrix 
## from cache. The functions require the matrix supplied to be invertible.

## The first function creates a "special" matrix object that contains a list of
## four functions to set the matrix, get the matrix, set the inverse of the matrix,
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
     
}


## The second function checks whether the inverse of the "special" matrix object 
## returned by the makeCacheMatrix function above had been calculated before, 
## in which case it retrieves the stored inverse matrix from cache, or else, 
## it computes it and stores it in cache using the four functions listed in 
## the "special" matrix object.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}


## Below an example for the use of the functions:

a <- matrix(c(5,4,6,7),2,2)
matr <- makeCacheMatrix(a)
cacheSolve(matr)




