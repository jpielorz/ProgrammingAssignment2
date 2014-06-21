## This R script contains a pair of functions, makeCachematrix and cacheSolve, which create a special "matrix"
## object and compute its inverse. If the inverse of the matrix has already been calculated before, the 
## inverse is retrieved from the cache. For this assignment we assume the matrix to be square invertible and use the solve(x) function for
## inversion.

## The first function creates the special "matrix", It contains a list of functions that allow one
## either to set or get the values of the matrix or to set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialization of the object for storing the inverse matrix
      
      i <- NULL
      
      ## Set matrix to be inversed. To access the objects from other environments, the <<- operator
      ## is used.
      
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      
      ## Set inverse matrix
      
      setinverse <- function(inv) i <<- inv
      
      getinverse <- function() i
      
      ## Return list with four functions.

      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The second function uses the "matrix" created with makeCacheMatrix and calculates its inverse if it
## has not been computed before. Otherwise, it gets the inverse from the cache and skips its computation.

cacheSolve <- function(x, ...) {
      
      ## Check whether the inverse has already been calculated and throw message if there is cached data.
      
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      ## Assign cached data
      
      data <- x$get()
      
      ## Invert matrix and cache the result via the setinverse
      
      i <- solve(data)
      x$setinverse(i)
      
      ## Return a matrix that is the inverse of 'x'
      
      i
}
