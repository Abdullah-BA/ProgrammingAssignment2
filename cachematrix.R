
# The function makeVector creates a special “matrix” object which
# is really a list containing a function to set and get the value 
# of the vector and the mean. The "matrix" object caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The function cachSolve computes the object returned by makeCacheMatrix. 
# Computing the inverse of a square matrix can be done with the solve function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() # return a matrix that is the inverse of x.
  if (!is.null(i)) {
    message("retreiving cached")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
