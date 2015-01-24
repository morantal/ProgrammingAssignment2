## functions cache martix's inverse, and if needed calculates the inverted marix and then caches it.

## function caches the inverted matrix in a special "Vector" containing set,get,setinverse,getinverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setmean = setinverse,
       getmean = getinverse)
  
}


## function checks if there is an inverted marix, if so retrieves it, if not calculates it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
