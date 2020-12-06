## This functions will cache the inverse of a matrix
## creating it in makeCacheMatrix, then passing the
## matrix to cacheSolve to get the inverse matrix.

## this function will get a matrix and set all the functions
## using the scoping rules for enviroment.
## test-it with this example
## x = makeCacheMatrix(matrix(c(2,1,4,-1,-1,-5,3,1,2),3,3))
## x always has to be a matrix, if is not a matrix you will
## get an error message.
## here is a explination of the functions within the enviroment:
## set: reset the matrix stored in this function enviroment.
## get: return the matrix stored
## setinvertida: set the inverse, which is calculated in the cacheSolve function.
## getinvertida: return the inverse matrix.
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

## This function take the matrix and send it to makeCacheMatrix to calculated
## the inverse of it, if it's already calculated return the value, otherwise the
## function calculated the inverse an return it.
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
