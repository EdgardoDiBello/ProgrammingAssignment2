## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  invertida <- NULL
  set <- function(y) {
    x <<- y
    invertida <<- NULL
  }
  get <- function() x
  setinvertida <- function(inver) invertida <<- inver
  getinvertida <- function() invertida
  list(set = set, get = get,
       setinvertida = setinvertida,
       getinvertida = getinvertida)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertida <- x$getinvertida()
  if(!is.null(invertida)) {
    message("Se consigio la inversa de la matriz")
    return(invertida)
  }
  data <- x$get()
  invertida <- solve(data, ...)
  x$setinvertida(invertida)
  invertida
}
