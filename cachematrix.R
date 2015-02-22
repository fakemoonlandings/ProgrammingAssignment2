## this function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # set i to NULL on creation
  i <- NULL
  
  # set - initial creation and setting of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get - return the matrix
  get <- function() x
  
  # setInverse - this is where we'll assign the value to "i" and cache it by putting it in a different environment
  setInverse <- function(inverse) i <<- inverse
  
  # getInverse - this is where we'll get the value, note that we set it to NULL when the object is created 
  getInverse <- function() i
  
  # this list exposes the functions we've created in our object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## this function takes a "makeCacheMatrix" object as a argument, then inverts it using Solve
## if the "makeCacheMatrix" object has already been used, the values are cached

cacheSolve <- function(x, ...) {
  
  # see if we have a value
  m <- x$getInverse()
  
  # if so, we can just return the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if not, get the matrix value
  data <- x$get()
  
  # then Solve it (invert)
  m <- solve(data, ...)
  
  # set the value inside our object
  x$setInverse(m)
  
  # and finally return that value (using the local environment rather than the object to save time)
  m
}
