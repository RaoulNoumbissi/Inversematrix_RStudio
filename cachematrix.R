## This program creates a matrix and cache its inverse.

## The makeCacheMatrix function below cache a square matrix in the set() function, then gets the matrix in the get() function.
## The makeCacheMatrix also chache the inverse of the matrix (setInverse), and gets te inverse of the matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
         }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
                        setInverse = setInverse,
                       getInverse = getInverse)

}


## The cacheSolve function below gets and return the inverse of the cache matrix if it exists, otherwise it calculates the inverse of the matrix (Solve), put it into cache, and return it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
      
}
