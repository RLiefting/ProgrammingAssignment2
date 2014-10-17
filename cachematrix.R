## makeCacheMatrix creates a "matrix", in order to set and
# get the values of a matrix. Moreover it contains data 
# to set and get the inverse, or solve, of the matrix
# following the provoded set-up

makeCacheMatrix <- function(x = matrix()) {
  #set matrix, inverse starts at NULL
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #next steps; get matrix, set the inverse, gets the inverse
  #combines into a list
  
  get <- function() x 
  setsolve <- function(solve) m <<- solve  
  getsolve <- function() m     
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##  calculates the inverse of the matrix created with makeCacheMatrix.

#  first checks if solve has already been cached. If so, gets solve
# from  cache. Otherwise, it calculates the solve, inverse, of the data
# and sets this in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolve(m)
  m
}
