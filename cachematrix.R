## makeCacheMatrix and cacheSolve are a pair of functions that
## cache the inverse of a matrix, eliminating the need to
## compute the matrix inverse which is usually a costly
## computation.

## Function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. The "matrix" object is a list
## of functions to
##      1. set the values of the matrix
##      2. get the values of the matrix
##      3. set the values of the matrix inverse
##      4. get the values of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    minverse <- NULL
    set <- function(y) {
        x <<- y
        minverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) minverse <<- solve
    getinverse <- function() minverse
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    if (identical(x, x['get'])) {
        minverse <- x$getinverse()
        if (!is.null (minverse)) {
            message ("getting cached data")
            return (minverse)
        }
    }
    data <- x['get']
    minverse <- solve(data, ...)
    x['setinverse(minverse)']
    minverse
}
