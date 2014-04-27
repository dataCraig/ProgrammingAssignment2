# The functions allow you to create a matrix which can store it's 
# inverse.
# makeCacheMatrix is a special function that creates a list of functions
# used by the cacheSolve function. Creates a cache and returns a list
# of functions used by cacheSolve.
# cacheSolve is passed a makeCacheMatrix and determines if a cached inverse
# exists and retrieves it or calculates the inverse if it doesn't.

# To use the functions create a cache using makeCacheMatrix as shown
# a = matrix(1:4, 2, 2)
# my_cache <- makeCacheMatrix()
# cacheSolve(a)  # Calculates the inverse and add it to the cache.
# cacheSolve(a)  # Retrieves the previously calculated value.

## Write a short comment describing this function
# Special function that accepts a matric as an argument and returns
# a list of functions that can be used to set and access the cache.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inverse <<- solve
    getSolve <- function() inverse
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## Write a short comment describing this function
# Accepts a cache, first checking if a cached solution exists and
# if not then finds the inverse, caches it and returns the inverse matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m
}

