## Put comments here that give an overall description of what your
## functions do

## Uses a variable 'cached_inv' to store the results of computing the
# inverse of a matrix for later use

makeCacheMatrix <- function(x = matrix()) {
   #define the "global" variable cached_inv
  cached_inv <- NULL
  # set operator used to give x a new value, since x is new,
  # the cached_inverse has to be reset to NUll
  set <- function(y) {
    x <<- y
    cached_inv <- NULL
  }
  # returns the matrix that was provided as input
  get <- function() x
  # sets the cached inverse to a specific value
  setinv <- function(inv) {
    cached_inv <<- inv
  }  
  # gets the cached inverse
  getinv <- function() cached_inv
  
  # returns the cached matrix data structure
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Computes the inverse of a matrix represented by a cached matrix
# x is a cachedMatrix created by makeCachedMatrix(x) for a matrix x

cacheSolve <- function(x, ...) {
        # get the cached inverse, if it's not null, return it
        inv <- x$getinv()
        if (!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
        }
        # if the cached inverse is NULL, compute and cache the result before returning
        message("computing inverse")
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}
