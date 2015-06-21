#The makeCacheMatrix function sets the inverse matrix to Null.
#Sets x to matrix input by the user
#Gets the matrix details
#Sets and gets inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#CacheSolve reads the matrix provided, checks if the inverse is already calcuated, if yes, 
#retrieves value from the cache, else calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  #m <- getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  #data <- get()
  inv = solve(data)
  x$setinv(inv)
  #setinv(m)
  inv
}