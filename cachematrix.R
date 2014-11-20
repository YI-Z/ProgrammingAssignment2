## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    changed <- FALSE
    set <- function(y) {
        x <<- y
        inv <<- NULL
        changed <<- TRUE
    } 
    get <- function() x
    setinverse <- function(i) {
        inv <<- i
        changed <- FALSE
    }
    getinverse <- function() inv
    isChanged <- function() changed
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, isChanged = isChanged)
    
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv) & !x$isChanged()) {
        message("Get cache data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
