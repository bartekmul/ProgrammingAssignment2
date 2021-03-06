##     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##     cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##     We assume that the matrix supplied is always invertible, and so it has to be square matrix

##     Here we catch inverse of the matrix and the matrix itself. 
##     Also we can change matrix, and add it's inverse computed somewhere else.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Here we compute the inverse of the matrix if it hasn't already been calculated, in other case we return this inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
