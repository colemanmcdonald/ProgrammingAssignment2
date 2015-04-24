## Put comments here that give an overall description of what your
## functions do

## These functions make, test and solve a cache for a matrix. 
## It will return the inverse of the matrix or, it will output 
## the previously evaluated solution if that matrix has already been evaluated

## Write a short comment describing this function

## makeCacheMatrix takes a matrix and creates a special vector which is a list 
## containing functions to set the value of the matrix, get the value of the matrix
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  xinv <- NULL 
  set <- function(y) 
  {
    x <<- y
    xinv <<- NULL 
  }
  get <- function() x 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv # return the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function

## cacheSolve calculates the mean of the special vector created in makeCacheMatrix 
## First, it checks to see if the matrix has already been solved.
## If so, it outputs the inverse 
## If not, it calculates it and sets it via setinverse

cacheSolve <- function(x, ...) 
{
  m <- x$getInv()
  if(!is.null(m)) 
  { 
    message("retrieving cached data")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}
