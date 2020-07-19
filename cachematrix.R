## R pRogramming Assignment 2
## We will use the ability of R to cache data.The first function creates a special 'matrix' object capable of caching its inverse

## The second function will be used for inversing the first.

makeCacheMatrix <- function(x = matrix()) {
  
    im <- NULL
    set <- function(y) {
      x <<- y
      imatrix <<- NULL
    }
    get <- function() x
    setimatrix <- function(imatrix) im <<- imatrix
    getimatrix <- function() im
    list(set = set, get = get,
         setimatrix = setimatrix,
         getimatrix = getimatrix)
  

}


## For this second function,if makeCacheMatrix can be inversed, it will either inverse that matrix using the Solve function or, if the inverse value has already been computed and the matrix is unchanged

cacheSolve <- function(x, ...) {
  im <- x$getimatrix()
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  matrix <- x$get()
  im <- solve(matrix, ...)
  x$setimatrix(im)
  im
  
}

