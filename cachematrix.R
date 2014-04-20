
## Make cache matrix, get ( set ) inv.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(newfunction)
  {
  x <<- newfunction
  p <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
  setinv = setinv,
  getinv = getinv)
  }

## cachesolve - function - inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (is.null(inv)) 
  {
  message("MISS- CACHE")
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  } 
  else
  message("HIT- CACHE")
  inv
  
  }
 
