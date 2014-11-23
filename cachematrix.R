##makeCacheMatrix: This function creates a "matrix" object and caches its inverse.
## m is put in the parent environment (superassignment <<) to be able to use it in the second function
##the function returns a list of 4 "method" objects.

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
          set <- function(y) {
          x <<- y
          m <<- NULL  }
    
    get <- function() {x}
    
    setInverse <- function(solve) {m <<- solve}
    
    getInverse <- function() {m}
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#This function computes the inverse of the"matrix" created by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#the function retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
