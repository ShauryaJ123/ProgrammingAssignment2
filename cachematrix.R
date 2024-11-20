## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


# Example usage
mat <- matrix(c(2, 1, 1, 3), 2, 2)
specialMatrix <- makeCacheMatrix(mat)
inv1 <- cacheSolve(specialMatrix) # Computes the inverse
inv2 <- cacheSolve(specialMatrix) # Retrieves the cached inverse
print(inv1)
print(inv2)
