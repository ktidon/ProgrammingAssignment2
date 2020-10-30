######################################################################################
## Computation for Matrix Inversion is usually a costly one. Here we will create a  ##
## set of functions that will solve the inverse of a Matrix and store it in cache   ##
## so that it just returns the stored result instead of computing it again          ##
######################################################################################

## This function  

makeCacheMatrix <- function(x = matrix()) {

  
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    get <- function(){ 
      x
      }
    
    
    setInverse <- function(inverseMatrix){ 
      inverse <<- inverseMatrix
      }
    
    
    getInverse <- function() {
      inverse
      }
    
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) { ##
    
    message("Retrieving cached data...")
    return(inverse)
  }
  
  Matrix <- x$get()
  
  inverse <- solve(Matrix, ...)
  
  x$setInverse(inverse)
  
  inverse
  
}

