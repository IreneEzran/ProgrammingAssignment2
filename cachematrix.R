##Matrix multiplication can be slow for computers to calculate, so it is best for the computer to calculate it once and cache it.

##makeCacheMatrix creates a list to set and get the value of a matrix and set and get the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ##set the matrix
    set <- function(y) {
    x <<- y
    i <<- NULL
    }
  ##get the matrix
  get <- function() x
  ##set the inverse
  setinverse <- function(inverse) i <<- inverse
  ##get the inverse
  getinverse <- function() i
  ##return a list of the methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##if the inverse is already calculated, it should be retrieved with cacheSolve
cacheSolve <- function(x, ...) {
    ##return the inverse matrix of x
    i <- x$getinverse()
    ##if it is already set, return the inverse
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    ##get the matrix from our object
    data <- x$get()
    ##matrix multiplication
    i <- solve(data, ...)
    ##set the inverse to the object
    x$setinverse(i)
    ##return the matrix
    i
  }