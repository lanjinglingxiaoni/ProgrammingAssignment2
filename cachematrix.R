
## This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set  <- function(y){
    x  <<- y
    invM <<- NULL
  }
  get  <- function() x
  setinvMatrix  <- function(inv_Matrix) invM  <<- inv_Matrix
  getinvMatrix  <- function() invM
  list(set = set, get= get,
       setinvMatrix = setinvMatrix,
       getinvMatrix = getinvMatrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getinvMatrix()
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)         
  }
  data <- x$get()
  invM <- solve(data, ...) 
  x$setinvMatrix(invM)
  invM
}
