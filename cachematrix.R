## These functions allow to create a special matrix whose
## inverse value can be cached.
## The function 'makeCacheMatrix' creates the matrix, while the
## function 'cacheSolve' calculates the inverse of the matrix if
## it's not stored in the special matrix.

## This function creates a special "matrix" object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # Inverse
  set <- function(y) { # Change the the matrix
    x <<- y
    i <<- NULL # Inverse should be calculated again
  }
  get <- function() { # Return the matrix
    x
  }
  setinverse <- function(inverse) { # Set the inverse
    i <<- inverse
  }
  getinverse <- function() { # Return the inverse or NULL if it's not set
    i
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # Get inverse
  if (!is.null(i)) { # Check if the inverse is already calculated
    message("getting cached data")
    return(i) # Return cached inverse
  }
  data <- x$get()
  i <- solve(data, ...) # Calculate inverse
  x$setinverse(i) # Store inverse to use later
  i # Return inverse
  
}
