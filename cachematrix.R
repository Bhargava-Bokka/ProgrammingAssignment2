## I've made use of the logic used in the example, to create a matrix first 
## using makeCacheMatrix function and then retrive the inverse if it already
## exists or calculate the inverse if it doesn't using cacheSolve function 

## This function take a matrix input and and returns a list containing functions
## to set the matrix, get the matrix, set the inverse of the matrix, and get the
## inverse ofthe matrix. 

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) I <<- inverse
      getInverse <- function() I
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function calculates the inverse of the matrix object returned by the 
## above function, by retrieving the inverse from cache if it is already
## calculated, or by calculating the inverse and storing it in the cache

cacheSolve <- function(x, ...) {
      I <- x$getInverse()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data, ...)
      x$setInverse(I)
      I
}
