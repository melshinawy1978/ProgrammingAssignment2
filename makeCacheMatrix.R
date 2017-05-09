#This function creates a special "matrix" object that can cache its inverse.
#--------------------------------------------

makeCacheMatrix <- function(x = matrix()) {  # this is the function that will be called
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) m <<- ginv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
#---------------------------------------------
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  library(MASS)
  m <- ginv(data, ...)  # ginv calculates the inverse of square and non square matrices
  x$setinv(m)
  m
  }
