##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(M) {
  I <- NULL
  set <- function(y) {
    M <<- y
    I <<- NULL
  }
  get <- function() M
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}