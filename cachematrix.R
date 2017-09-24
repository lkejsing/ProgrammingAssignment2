## The two functions create an object that stores a matrix and caches its inverse.
## The first function creates an object that stores a matrix. The second function uses this object to 
## compute the inverse of the matrix and cache the value in the object's environment. 

## This function takes a matrix as input and creates a list of four functions as output. 
## These functions are used by the cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function takes the output of the first function as input.
## It first checks to see if the inverse of the matrix has been cached. If it has, it returns 
## the cached inverse matrix. If not, it gets the value of the matrix x (input to the first function) 
## and calculates the inverse of this. It then sets the value of the inverse matrix to the first 
## function's environment and returns the inverse matrix.

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}