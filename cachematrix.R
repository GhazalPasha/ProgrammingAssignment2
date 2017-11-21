## The makeCacheMatrix function create get, set, getinv and setinv functions for the matrix.
## The cachesolve function use getinv to check if the matrix inverse is already computed.
## If yes, it returns the cachesd data.
## If no, it calcualte and return the matrix inverse and save it using setinv.

## makeCacheMatrix creates a list of functions to cache the matrix inverse
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

## cachesolve cache the matrix inverse for a repeated matrixor
## or calculate the matrix inverse for a new matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x'
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
