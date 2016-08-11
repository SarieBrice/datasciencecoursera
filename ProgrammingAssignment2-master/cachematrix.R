## Coursera Data science Assignment 2
## This assignment consists of two functions called "makeCacheMatrix" and "cacheSolve"

##"makeCacheMatrix" will create a matrix. It containts a list of functions:
##1. a function to set a value of the matrix
##2. a function to get the value of the matrix
##3. a function to set the inverse of the matrix 
##4. a function to get the inverse of the matrix
##The function only compute the inverse if the matrix is not singular, i.e.
##when the determinant of the matrix is non-zero. Otherwise, it will print
##a reminder that the input matrix is a singular.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  if(det(x) != 0){
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }
  else {print ("The matrix is singular.")}
}


##The funtion "cacheSolve" computes the inverse of the matrix stored in the list.
##It will check if the inverse matrix has been calculated, then will return
##the stored inverse matrix. Otherwise it will compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
