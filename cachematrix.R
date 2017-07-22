
## My objective is to cache the inverse of a matrix with the help of two functions:
## 1. makeCacheMatrix
## 2. cacheSolve

## Assumption - The matrix is always invertible.

## makeCacheMatrix function will create the special object to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function will compute the inverse of a matrix obtained as an output using the above special object.
## If the inverse is already computed, then this function retrieves that inverse, provided that the matrix remains unchanged.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
