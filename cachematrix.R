## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates an object (list) which stores a matrix, its inverse and contains 
##                  funtions to get and set these as follows
##                  get()              - returns the matrix
##                  set(matrix)        - sets the matrix
##                  getinverse()       - returns the inverse (or NULL if not already set)
##                  setinverse(matrix) - sets the inverse
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL 
 ## define functions
 get <- function() x
 getinverse <- function() inv
 set <- function(matrix) {
   x <<- matrix
   inv <<- NULL
 }
 setinverse <- function(matrix) inv <<- matrix
 ## return list of functions
 list(get = get,
      getinverse = getinverse,
      set = set,
      setinverse = setinverse)
}


## Write a short comment describing this function
## cacheSolve: returns the matrix inverse of a object (list) created by makeCacheMatrix
##             if the inverse has already been calculated the return its cached value,
##             if not then calculate caclculate the inverse with solve(), set the cached value and return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## return cached inverse if already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## calculate inverse, set cache and return
  matrix <- x$get()
  inv <- solve(matrix, ...) # ... allows extra arguments to be passed to solve() if desired
  x$setinverse(inv)
  inv
}