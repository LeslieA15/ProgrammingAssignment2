# These comments/structures/naming conventions follow the discussion examples
# and flow by Alan E. Berger on Coursera website.
#
# A simple matrix m1 with a simple matrix inverse n1 
#
# This assignment focuses on saving the inverse n1 in cache 
# for reuse and avoid significant computational resources 
# and time to redetermine
#
# I wasn't sure about what graders would do at the console level
# so in the below, where you see ####, I'm assuming you'll type
# those specific commands to run and see the functions work.
#
# The below programs were tested with matrix m1 as:
#
#        0   1   2 
#        1   0   3
#        4  -3   8
#
# m1 <- matrix(c(0, 1, 4, 1, 0, -3, 2, 3, 8), nrow = 3, ncol =3)
# The expected inverse matrix n1 would then be:
#
#     -4.5  7  -1.5
#     -2.0  4  -1.0
#     1.5  -2   0.5

####   rm(list=ls()) I like to clear the variables before getting started :-)
####   Source("cachematrix.R")
####   m1 <- matrix(c(0,1,4,1,0,-3,2,3,8), nrow = 3, ncol = 3)
####   myMatrix_object <- MakeCacheMatrix(m1)
# and then
####   cacheSolve(myMatrix_object)
# should return exactly the n1 inverse matrix above
# Running 
####   cacheSolve(myMatrix_object)
# should return n1 again but with a message saying
#"getting cached inverse matrix"
# 
# you can use the set function to "put in" a new matrix
# for example n2
####  n2 <- matrix(c(5/8,-1/8,-7/8,3/8), nrow = 2, ncol = 2)
####  myMatrix_object$set(n2)
# This should redefine the matrix to be inverted 
# and resets the indicator for having already
# calculated the inverse to NULL,
# without needing to call makeCacheMatrix again
#
# we can obtain its matrix inverse by
#### cacheSolve(myMatrix_object)
# The new n1 inverse should be
#      3  7
#      1  5
# Once again, the new inverse matrix should be stored as 
# n1 in cached memory and accessible for reuse without 
# recalculation
#### cacheSolve(myMatrix_object)
# should return the 2x2 inverse in n1 with a message saying
# "Getting cached inverse matrix"

makeCacheMatrix <- function(x = matrix()) 
{    # start makeCacheMatrix function to create/modify myMatrix_Object list
  
  n1 <- NULL
  
  set <- function(y) # when user resets matrix to be inverted, do a reset 
  {  
    x <<- y       # set x$set to new matrix
    n1 <<- NULL   # reset n1 to reflect nothing yet in cache
  } 
  #
  get <- function() x # get the matrix to be inverted
  #
  setinverse <- function(inversem) n1 <<- inversem # set n1 to be inverse matrix
  #                                             <<- means it will be in cache
  #
  getinverse <- function() n1     # if an inverse is cached, this will retrieve
  #
  # rename x list headings to these so we can use myMatrix_Object$set and so forth
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
  
}   # end makeCacheMatrix function

cacheSolve <- function(x)
{
  n1 <- x$getinverse()
  if(!is.null(n1))
  {
    message("Getting cached inverse matrix")
    return(n1)
  }
  print("start get data")
  data <- x$get()
  n1 <- solve(data)
  x$setinverse(n1)
  n1
}