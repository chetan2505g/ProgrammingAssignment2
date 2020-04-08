## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Created by Chetan

# We have written 4 following method
#set: Which set the value 
#get: Which get the value
#setInverse: set the inverse value of the matrix
#getInvers: get inverse value of the matrix
# When we pass the matrix. You can run following command and check result
# x <- matrix(1:4,2,2)
# m1 <- makeCacheMatrix(x)
# For method: m1$get()

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
#Below function will check if function in cache or not if it is available in cache then 
#it will fetch from cache other wise it will run internally

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

