######################################################################################
## Computation for Matrix Inversion is usually a costly one. Here we will create a  ##
## set of functions that will solve the inverse of a Matrix and store it in cache   ##
## so that it just returns the stored result instead of computing it again          ##
######################################################################################

# This function will create a special matrix object and will cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # This initializes the inverse of the matrix
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


# This function checks if the inverse of the matrix has already been computed.
# If not, it computes it and stores it in cache for future reference.

cacheSolve <- function(x, ...) {


  inverse <- x$getInverse()

  if (!is.null(inverse)) {

  # If this returns true, it means the inverse has already been computed
  # otherwise, it will compute for it

    message("Retrieving cached data...")
    return(inverse)
  }

  # This gets the matrix and stores it in "Matrix"
  Matrix <- x$get()

  #  This solves for the inverse
  inverse <- solve(Matrix, ...)

  x$setInverse(inverse)

  inverse

}
