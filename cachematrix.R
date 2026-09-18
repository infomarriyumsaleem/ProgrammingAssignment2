## These functions create a special matrix object that can store
## a matrix and cache its inverse. The inverse is calculated only
## when it is not already available in the cache.

## This function creates a special matrix object with functions
## to set and get the matrix and to store and retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  getinverse <- function() {
    m
  }
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function returns the inverse of the special matrix.
## If the inverse is already cached, it returns the cached
## value instead of calculating the inverse again.

cacheSolve <- function(x, ...) {
  
  ## Check whether the inverse is already cached
  m <- x$getinverse()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Calculate the inverse if it is not already cached
  data <- x$get()
  m <- solve(data, ...)
  
  ## Store the calculated inverse in the cache
  x$setinverse(m)
  
  ## Return the inverse
  m
}
