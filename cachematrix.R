## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). Your assignment is 
## to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## set the value of the vector
## get the value of the vector
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setIn <- function(inverse) m <<- inverse
  
  
  getIn <- function() m
  
  list(set = set, get = get,
       setIn = setIn,
       getIn = getIn)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {
  
  m <- x$getIn()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve (data, ...)
  
  x$setIn(m)
  
  return (m)
}
