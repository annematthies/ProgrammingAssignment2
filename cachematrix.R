## The functions solve a given invertible square matrix and cache it such that the 
## inverse can be looked up in the cache and do not need to be recomputed again.

## makeCacheMatrix creates a "special" matrix which is a list of four functions.
## 1. get : returns the original function
## 2. set : sets and caches the matrix, sets and caches the inverse to NULL
## 3. setinverse : solves the matrix and caches the resulting inverse
## 4. getinverse : returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a matrix from the type of the special matrix
## which can be gained by the function above.
## It looks up if the inverse is already in the cache. If so it gets the cached data.
## Otherwise, the inverse is computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
