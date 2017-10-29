## This is Charles Breeden's solution for programming assignment #2
## R Programming course in Coursera

## This file contains two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     
     ## Set the matrix
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     ## Get the matrix
     get <- function() x
     
     ## Set the inverse matrix
     setinverse <- function(inverse) i <<- inverse
     
     ## Get the inverse matrix
     getinverse <- function() i 
     
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
     
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     
     if(!is.null(i)) {
          message("getting cached matrix")
          return(i)
     }
     
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}