## These kinds of functions will store a function's input
## Matrix object to create cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function below gives the inverse of the matrix. 
## First checks to see if the inverse has already been calculated, then it gets the outcome and skips the
## calculation. Alternatively, it adds the inverse and sets the value in the cache via the set inverse function.

## This function is based on the assumption that the matrix is always invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("obtain cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}

