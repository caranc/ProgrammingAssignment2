# R Programming - Programming Assigment 2: Lexical Scoping
# Class website: https://class.coursera.org/rprog-015

# Example:
# > m <- makeCacheMatrix()
# > m$set(matrix(c(4,2,7,6), 2, 2))
# > m$get()
# > cacheSolve(m)
# > cacheSolve(m)

# Output:
# > m <- makeCacheMatrix()
# > m$set(matrix(c(4,2,7,6), 2, 2))
# > m$get()
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

# makeCacheMatrix function creates functions to:
# 1. Set the value of matrix
# 2. Get the value of matrix
# 3. Set the value of inverse
# 4. Get the value of inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL                                  # Initialize inv to NULL for store inverse matrix 
  set <- function(y) {                         # Create function to store matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                          # Create function to get matrix 
  setinv <- function(inverse) inv <<- inverse  # Create function to set value of inverse in cache
  getinv <- function() inv                     # Create function to get value of inverse in cache
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function calculate the inverse of matrix created with makeCacheMatrix function
## Before calculation, it first checks if the inverse has already been calculated
## If so, it gets the inverse from the last time cache and skip the computation.
## Otherwise, it calculates the inverse of matrix and sets the inverse in cache via setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()                   # Call function getinv to get value from cache 
  if (!is.null(inv)) {                # Check inv
    message("getting cached data")    # If inverse has been calculated - inv is not NULL,    
    return(inv)                       # show message and get inverse from cache
  }                                   # If inverse has not calculated - inv is NULL, 
  data <- x$get()                     # Call function get in makeCacheMatrix
  inv <- solve(data, ...)             # Calculate inverse matrix
  x$setinv(inv)                       # Call function setinv to store inverse matrix  to cache
  inv
}