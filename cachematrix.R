## Programming Assignment 2: Lexical Scoping
## cachematrix.R

## Function 'makeCacheMatrix' creates a matrix object that can cache
## its inverse.

## Function 'cacheSolve' computes the inverse of the matrix returned by
## makeCacheMatrix.  If the inverse has been calculated, then cacheSolve will
## retrieve the inverse for the cache.


## Function to create a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x= matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Function to compute the inverse of the special 'matrix' returned by
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Test Cases
##
## Load the procedure
## > source("cachematrix.R")
##
## Create a matrix
## > c <- makeCacheMatrix(matrix(1:4,2,2))
##
## Display the matrix you just created
## > c$get()
##
## Generate the inverse of the matrix
## cacheSolve(c)
##
## Try to generate the inverse a second time.  cacheSolve returns cached data
## cacheSolve(c)
##
## End of cachematrix.R
