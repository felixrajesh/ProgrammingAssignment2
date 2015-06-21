## This R code has two functions named makeCacheMatrix() & cacheSolve(). 
## makeCacheMatrix() Function creates a matrix object and can cache its inverse
## cacheSolve() functions computes the inverse of the "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated and matrix has not changed, then the 
## cacheSolve should retrive the inverse from the cache.

## makeCacheMatrix() has 4 sub functions. 
## get() fucntion gets the input for the makeCacheMatrix()
## set() function set a new matrix object y to the x and return a null value. 
## SetInv() function getting an input and storing it as inverse matrix(matInv). 
## It is not calculating actual Inverse of matrix.
## getInv() just give out the set Inverse matrix.

makeCacheMatrix <- function(x = matrix()){
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInv<- function(mi) matInv <<- mi
  getInv<- function() matInv
  list(set = set, get = get,
       setInv=setInv,
       getsInv=getInv)
}

## cacheSolve() function take the returned matrix value from the makeCacheMatrix()
## Here the Inverse of the inptu matrix is calculated actually through the solve()function
## Thie function checks for the previously stored inverse matrix value, whether it 
## is null or not. If it is not null then it shows a message "getting cached data".
## it caches the previously stored data and return the value, else it calculates the
## matrix inverst and return the same.

cacheSolve <- function(x, ...) {
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  matInv <- solve(data, ...)
  x$setInv(matInv)
}
