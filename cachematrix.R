## this file finds the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
## this is where the parent function is created
## matrix entered here should be invertible
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) {m <<- Inverse}
  getInverse <- function() {m}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m    
  
}
