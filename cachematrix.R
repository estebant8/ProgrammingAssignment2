## Put comments here that give an overall description of what your
## functions do.
## Sometimes we will face problems with a very extense length
## then recomputing a function may result unefficient. Therefore,
## this functions look up for imformation in cache, this only 
## works if the content of our vector/matrix is not changing

## Write a short comment describing this function
## this function creates a special matrix which contains a series 
## of instructions, first it sets the value of the matrix
## second, it gets the values of the matrix; third, it sets the 
## value of the inverse and fourth it gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  l <- NULL
  set <- function(y){
    x <<- y
    l <<- NULL
  }
  get <- function() x
  set_inverse<- function(inverse) l <<-inverse
  get_inverse<- function() l
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}

## Write a short comment describing this function
## This other function calculates the inverse of the special matrix created 
## in the previous function. The very first thing it does is to chec if the
## inverse has been already calculated. If that is true, then the function 
## gets the inverse from the cache. If not, then it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  l <- x$get_inverse()
  if (!is.null(l)){
    message("looking for data at cache")
    return (l)
  }
  data <- x$get()
  l <- solve(data, ...)
  x$set_inverse(l)
  l
}