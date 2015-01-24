
## This function creates a matrix object that caches its inverse 
## using the solve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function computes the inverse of the matrix returned by 
## the above function. If the inverse has already been calculated, 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- invert(data, ...)
  x$setinvert(m)
  m
}
