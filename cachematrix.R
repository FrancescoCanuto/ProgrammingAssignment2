## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  # get the matrix
  get <- function() x                               
  # Inverse evaluation
  setinversematrix <- function(solve) xinv <<- solve 
  # Get inverse of matrix
  getinversematrix <- function() xinv                  
  # the list of 4 functions is returned
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  xinv <- x$getinversematrix()
  #if the matrix xinv already inversed. If this is the case then cached matrix is returned
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  # if not available the inversion is performed
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinversematrix(xinv)
  xinv # return the xinv
}