## This script contains 2 functions, makeCacheMatrix and cacheSolve. The basic idea of these functions is to store the result of time consuming functions so that if the same result is required again, a similar calculation is not repeated. End goal - processor time is saved or reduced

## The makeCacheMatrix function takes a matrix as an argument and returns a list of functions that: sets the value of the matrix, gets the value of the matrix, sets the value of the inverse and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) i <<- solve
  get_inverse <- function() i
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## The cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached(stored) data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
  
}

# m <- matrix(c(1,2,3,4), nrow=2, ncol = 2)
# x <- makeCacheMatrix(m)
# cacheSolve(x)
# cacheSolve(x)
# cacheSolve(x)

