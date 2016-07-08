## These two functions work together to retrieve a
## cached inveres of a matrix. The first step is to
## create a matrix using the "makeCacheMatrix()" 
## function, assigning it to a variable, and then
## passing that variable to the "cacheSolve(matrix)"
## function in order to decide to calculate the inverse
## or retrieve the cached value

## This function returns a matrix that is cacheable
## This function includes an internal variable that
## will be assigned a value and stores that value in
## memory
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if there is a cached version of
## the inverse of the matrix. It will return the cached inverse
## if it exists, or it will calculate the inverse and then
## store it in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
