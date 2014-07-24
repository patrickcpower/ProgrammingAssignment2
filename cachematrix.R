## makeCacheMatrix() and cacheSolve() allow for the cacheing of the result of the
## inverse of a matrix. The makeCacheMatrix() function creates a list object with 
## accessor functions:
##    get() returns the matrix
##    set() sets the value of the matrix and sets the inverse to NULL
##          forcing the re-calculation of the inverse
##    setinverse() sets the cached value of the inverse
##    getinverse() gets the value of the inverse
##
## cacheSolve() takes the list "matrix" object as it's first parameter
## and optional parameters which will be passed to the solve() function.
## The function checks if the inverse cache variable is NULL. If true
## the solve function is called and the cache value updated and returned.
## If false, the cached value of the inverse is returned and a message is 
## output.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}

