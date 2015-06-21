
## 1. The functions mean to calculate the inverse of a matrix.
## 
## 2. Create a matrix to store the inverse of itself
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m<<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 3. Calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
