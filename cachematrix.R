## This functions will cache the inverse of a matrix
## creating it in makeCacheMatrix, then passing the
## matrix to cacheSolve to get the inverse matrix.

## this function will get a matrix and set all the functions
## using the scoping rules for enviroment.
## test-it with this example
## x = makeCacheMatrix(matrix(c(2,1,4,-1,-1,-5,3,1,2),3,3))
## x always has to be a matrix, if is not a matrix you will
## get an error message.

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

## this function take the matrix created by makeCacheMatrix
## and get the inverse of it using cacheSolve(x).
## with the example you will get this matrix
##    [,1]  [,2]  [,3]
##[1,]  3    -13    2
##[2,]  2      8    1
##[3,]  -1     6   -1

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
