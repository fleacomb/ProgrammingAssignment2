## This generates a list which is cached, to hold the inverse of the matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix stores the values for cacheSolve in a list.

makeCacheMatrix <- function(x = matrix()) {
    Mi <- NULL
    set <- function(y){
        x <<- y
        Mi <<- NULL 
    }
    get <- function () x
    setinv <- function(solve) Mi <<- solve
    getinv <- function() Mi
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will calculate the inverse for a matrix, but will check for the stored value first.
## If there is no cached data for the function solve, it will calculate the matrix inverse.
cacheSolve <- function(x, ...) {
  Mi <- x$getinv()
  if(!is.null(Mi)) {
    message("getting cached data")
    return(Mi)
  }
  matrix <- x$get()
  Mi <- solve(matrix, ...)
  x$setinv(Mi)
  Mi
        ## Return a matrix that is the inverse of 'x'
}
