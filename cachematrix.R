## The first function allows caching a matrix's inverse
## The second function will compute the inverse of the matrix - if it has
## already been calculated, it will pull it from the cache



## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


source("C:/Users/corey/ProgrammingAssignment2/cachematrix.R")
matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
matrix$get()

matrix$getInverse()

cacheSolve(matrix)