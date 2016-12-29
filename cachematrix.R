## Make Caché matrix calculates the inverse of a matriz and stores it into the current directory.
## it might be usefull because inverting a matrix can take time if the matrix is too big. 
## Both MakeCacheMatrix and cacheSolve are not used directly as function(matrix). The results of each step must be stored so an error does not appear.
## If cacheSolve is not used before looking makeCacheMatrix, the answer will be NULL: The inverted matrix does not exists. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## Cache solve "looks"if the matrix has been created already. If it is not, it creates it. If the matrix already exists, it does not calculate
## it again, but instead it "recalls"the existant inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
