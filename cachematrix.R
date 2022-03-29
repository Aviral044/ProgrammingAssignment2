##Week 3 Assignment 2

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                ## initialize j as NULL; will hold value of matrix inverse
  set <- function(y){      ## define the set function to assign new value of matrix in parent environment
    x <<- y
    j <<- NULL            ## if there is a new matrix, reset j to NULL
  }
  get <- function()x       ## define the get function which returns value of the matrix argument
  setInverse <- function(inverse) j <<- inverse  ## assigns value of j in parent environment
  getInverse <- function() j                     ## gets the value of j where called
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
