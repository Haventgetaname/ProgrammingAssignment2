## Put comments here that give an overall description of what your
## functions do

##This fuction creates a special "vector", which is a list containing a function to
##1.set the value of the Matrix
##2.get the value of the Matrix
##3.set the value of inverse of the martix
##4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <-function(inverse) inv<<-inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function calculates the inverse of the special martrix  created with the above function.
## it first checks to see if the inversed matrix has already been created.
## If so, it gets the inversed matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inversed matrix 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
          message("getting cached data")
        return(inv)
        }
        mat<- x$get()
        inv<-solve(mat,...)
        x$setinverse(inv)
        inv
}



