## makeCacheMatrix and cacheSolve are two functions 
## related to finding the inverse of a matrix and 
## caching the result for possible future use.



## makeCacheMatrix creates a matrix object capable 
## of caching its own inverse

makeCacheMatrix <- function(mymatrix = matrix()) {
  ## mymatrix is passed in.
  ## Initialize the inverse matrix to NULL
  ## (If makeCacheMatrix is being called, the inverse doesn't exist yet.)
  myinverse <- NULL
  
  ## Set the matrix
  ## (If matrix is being set, then inverse doesn't exist yet.)
  set <- function( matrix ) {
    ## Set mymatrix to the "matrix" passed in to set().
    mymatrix <<- matrix
    myinverse <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the already existing matrix.
    mymatrix
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    ## Set myinverse to the "inverse" matrix passed in to setInverse()
    myinverse <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the already existing inverse matrix.
    myinverse
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve first looks to see if the inverse 
## of a matrix has already been cached; if not, 
## then it creates the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Return a matrix that is the inverse of 'x'
  ## This exists if the inverse has been cached.
  myinverse <- x$getInverse()
  
  ## If the inverse is not NULL, then it has already been calculated
  ## and cached, so just return it,
  if( !is.null(myinverse) ) {
    message("Returning cached inverse.")
    return(myinverse)
  }
  
  ## Get the matrix from our object
  mymatrix <- x$get()
  
  ## If the inverse is NULL (i.e., doesn't already exist),
  ## then calculate the inverse.
  ## Reference used:  https://www.statmethods.net/advstats/matrix.html
  myinverse <- solve(mymatrix)
  
  ## The following should return the identity matrix, i.e., all 1's along the diagonal.
  ## Note:  * is element-by-element multiplication, %*% is matrix multiplication
  ##x <- solve(mymatrix) %*% mymatrix
  
  ## Set the inverse to the object
  x$setInverse(myinverse)
  
  ## Return the matrix
  myinverse

}
