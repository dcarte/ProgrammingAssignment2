## The following functions cache the inverse of a matrix rather than unnecessarily computing it repeatedly.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            
          i <- NULL
       set <- function( matrix ) {
           x <<- matrix
           i <<- NULL
         }
          get <- function() {
               x
           }
               setInverse <- function(inverse) 
               i <<- inverse
               getInverse <- function() {
                                 i
                                 list(set = set, get = get,
                                 setInverse = setInverse,
                                 getInverse = getInverse)
               }
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(i)
  i
} 
        
        

