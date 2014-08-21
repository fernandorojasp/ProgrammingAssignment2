## makeCacheMatrix creates a Cache of parameter x, being x a matrix
## Cache consists of a list with four elements:
## 1) set, function to set the matrix returned
## 2) get, function to get the matrix value
## 3) setinvmat, stores the inverse matrix
## 4) getinvmat, gets the inverse matrix (when stored before)
## It sets it all and keeps the list in current environment
## The function returns a list with the four elements indicated before
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
 
  setinvmat <- function (mat) m <<- mat
  
  getinvmat <- function () m
  
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## cacheSolve returns the inverse of the matrix 'x'
## It takes as income the Cache of matrix 'x' (passed before through makeCacheMatrix)
## Should the matrix 'x' have been passed before through this function,
## inverse calculation is not performed, but the value is taken and returned from the cached matrix 'x' (get the proper value in the list)
cacheSolve <- function(x, ...) {
  
  m <- x$getinvmat()
  
  if (!is.null(m)) {
    message("getting cached inv matrix")
    return (m)
  }
  
  data <- x$getinvmat()
  m <- solve(mat, ...)
  x$setinvmat(mat)
  m
}
