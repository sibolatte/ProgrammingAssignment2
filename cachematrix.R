##Let's create makeCacheMatrix and CacheSolve
## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##and cacheSolve function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Sample Example
## x = rbind(c(1,1/2), c(-1/2, 1))
## s = makeCacheMatrix(x)
## s$get()
##       [,1]  [,2]
## [1,]  1.00 0.50
## [2,] -0.50  1.00

## No cache in the first run
## cacheSolve(s)
## [,1] [,2]
## [1,]  0.8 -0.4
## [2,]  0.4  0.8
