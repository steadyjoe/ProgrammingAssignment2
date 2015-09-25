## R Programming
## Assignment 2: Caching the Inverse of a Matrix
## This second programming assignment will require you to write an R function is able to cache potentially time-consuming computations. 
## For example, taking the mean of a numeric vector is typically a fast operation. 
## However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 
## In this Programming Assignment will take advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Note, Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
cachesolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## Test these out with:
## a <- makeCacheMatrix(matrix(1:4, 2, 2))
## cachesolve(a)
## and then again!... to get the cached result...
## cachesolve(a)
