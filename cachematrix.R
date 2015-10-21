#makeCacheMatrix is a list containing 4 functions:
# 1- set the value of the matrix: changes the value of the 
# matrix stored in the main function (makeCacheMatrix)
# 2- get the value of the matrix: returns the value of the 
# matrix stored in the main function  
# 3- set the inverse of such matrix: does not calculate the 
# inverse, just stores the value of the input in a variable
# m into the main function (makeCacheMatrix)
# 4- get the inverse of such matrix: returns the value of 
# the m variable 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  # the value inverse is in theory the inverse of the 
  # matrix x, but this line really only stores a variable 
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function()m
  
  list(set=set, get=get, setinverse=setinverse,
      getinverse = getinverse)
}


# CacheSolve is a function that calculates the inverse 
# of the matrix stored in makeCacheMatrix: the input of
# CacheSolve is going to be the "object" created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    # looks for the value m from the previous function and
    # verifies that it exists in the memory and is not NULL : 
    # if ok then gives the message and returns the value.
    # so if it gets the inverse from the previous function
    # (has been already calculated) then gets the desired
    # value from the cache and skips all the calculation
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached matrix")
      return(m)
    }
    
    # but if the value is not in the cache then it goes on 
    # to calculate the inverse of the data and sets the 
    # value of the inverse in the cache via the
    # x$setinverse function
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}