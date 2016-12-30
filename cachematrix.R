## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
