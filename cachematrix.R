
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Thus, this file contains a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse. This function, creates a special "matrix", which is
## really a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	  m1 <- NULL

	  ## set() implements the function that sets the value of the matrix
	  set <- function(y) {
                x <<- y
                m1 <<- NULL
        }

	  ## get() implements the function that gets the value of the matrix
	  get <- function() x


	  ## setinverse() sets the value of the inverse 
	  setinverse <- function(inverseMatrix) { 
	  	  m1 <<- inverseMatrix
	  }

	  ## getinverse() implements the function that gets the value of the inverse of 
	  getinverse <- function() m1

	  ## returns a list of the defined set(), get(), setinverse() 
	  ## getinverse()
	  list(set=set,get = get,setinverse=setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. 
## Function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

	  ## Get the inverse of matrix from cache
	  a = x$getinverse()

	  ## Check if the inverse of matrix 'x' exist in cache
	  ## If inverse of 'x' exists in cache, return the inverse of 'x' 
	  if (!is.null(a)) {
	  	message("getting cached data")
		return(a)
	  }

	  ## Else calculate the inverse and store it in cache
	  b <- x$get()
	  c <- solve(b)
	  x$setinverse(c)

        ## Return a matrix that is the inverse of 'x'
	  c

}
