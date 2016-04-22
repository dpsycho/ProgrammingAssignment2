
# Function makeCacheMatrix
# Takes a matrix as an input and produces a list of the following functions:
#	1. Set the matrix
#	2. Get the matrix
#	3. Set the inverted matrix
#	4. Get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                       # Reset inverted matrix
  set <- function(y) {                            # function to set the original matrix
    x <<- y
    i <<- NULL                                    # Reset the inverted matrix since the original matrix may have been changed due to set, thus the cached information might not be correct
  }
  get <- function() x                             # Get original matrix
  setinverse <- function(inverse) i <<- inverse   # Set inverted matrix
  getinverse <- function() i                      # Get inverted from cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function cachesolve
# Takes as an argument a list produced in the makeCacheMatrix and produces the inverted matrix
# of the matrix contained in that list
# In case the matrix cannot be inverted an error is produced. 
# The result, either the inverted matrix or the error, is cached 

cachesolve <- function(x, ...) {
  i <- x$getinverse()               # Try getting inverted matrix from cache
  if(!is.null(i)) {                 # Inverted matrix is available in cache, no need to calculate it again
    message("getting cached data")
    return(i)                       # Return cached inverted matrix           
  }
  data <- x$get()                   # If inverted matrix is not available in cache, first get the matrix to be inverted
  
  i <- tryCatch( 
          solve(data, ...),         # Call solve to invert original matrix, use tryCatch to handle exceptions 
          warning = function(w)     # in case the original matrix is not invertible
          { cat('Warning in inversion of the Matrix\n'); 
            w
          }, 
          error= function(e) 
          { cat('Cannot invert Matrix\n'); 
            e
          } 
        )
  x$setinverse(i)                   # Cache the inverted matrix, or the error in case the matrix is not invertible
  i                                 # Return inverted matrix (or error in case the matrix is not invertible)
}