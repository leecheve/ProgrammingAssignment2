## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

mymat <- makeCacheMatrix(matrix(c(1, 8, -9, 7, 5,
                                  0, 1, 0, 4, 4,
                                  0, 0, 1, 2, 5,
                                  0, 0, 0, 1, -5,
                                  0, 0, 0, 0, 1), nrow=5, ncol=5, byrow=T))

mymat$get()
cacheSolve(mymat)
