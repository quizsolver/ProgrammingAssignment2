## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,     ## special "matrix" object
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve computes the inverse of the special "matrix" object returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("using cache")
    return(m) ## skip and print cached result
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
