## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#' This function implements matrix with catche.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse  <- function(inv) inverse <<- inv
  getinverse  <- function() inverse
  list(set = set, get = get,
       setinverse  = setinverse ,
       getinverse  = getinverse )
}


## Write a short comment describing this function
#' This function solves matrix. If matrix has been solved before, it uses catche.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
