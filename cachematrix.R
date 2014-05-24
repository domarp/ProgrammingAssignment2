## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##            makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##            has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

## This function is similar to the makeVector() function explained in the problem statement. The
## function returns a list with 4 functions:
##  1. set() : Initializes a matrix
##  2. get() : Get the matrix
##  3. setinv() : set the inverse of a matrix into the cache
##  4. getinv() : get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

##  This function is similar to the cacheMean() explained in the proble statement. This function
##  returns the inverse of the special matrix returned by the makeCacheMatrix function. If the 
##  inverse is present in the cahe it retrieves this inverse and returns it else the inverse is
##  computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
