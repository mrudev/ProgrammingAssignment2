## These two functions can be used to cache the inverse of a matrix
## instead of computing it repeatedly

## makeCacheMatrix creates a special "matrix", that can cache its inverse
## This is actually a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix created with makeCacheMatrix
## It fisrst checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache 
##	via the setinverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
