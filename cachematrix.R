## The first function of makeCacheMatrix is to create list of four functions
## 1. set the value of the list
## 2. get the value of the list
## 3. set the value of inversed matrix
## 4. get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function of cacheSolve is to calculate the inverse of the list 
## created by the first function. It first check whether inverse has been caculated.
## If so, it get the inverse from the cache and end the computation. Otherwise, it 
## calculates inverse of matrix and set the value of inversed matrix in the cache 
## by the setinverse() function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
