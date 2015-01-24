
### Introduction

## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. This assignment is to write a pair of functions that
## cache the inverse of a matrix.

### Example

## > source('cachematrix.R')
## > x <- matrix(1:4, 2, 2)
## > x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a <- makeCacheMatrix(x)
## > summary(a)
##        Length Class  Mode    
## set    1      -none- function
## get    1      -none- function
## setinv 1      -none- function
## getinv 1      -none- function
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > (inv <- cacheSolve(a))
## getting cached inverse matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > x %*% inv
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

### Functions

## Creates a special "matrix" object that can cache its inverse
## The input matrix is initially an empty matrix

makeCacheMatrix <- function(x = matrix())
{
        # Initializing the inverse matrix
        inv <- NULL
        
        # Set the matrix
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        
        # Get the matrix
        get <- function()
        {
                x
        }
        
        # Set the inverse matrix
        setinv <- function(inverse)
        {
                inv <<- inverse
        }
        
        # Get the inverse matrix
        getinv <- function()
        {
                inv
        }
        
        # Return a list including all inner functions above
        invisible(list(set = set, get = get,
                       setinv = setinv,
                       getinv = getinv))
}


## Computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...)
{
        # Get the inverse of matrix x from the cache
        inv <- x$getinv()
        
        # If the inverse has already been calculated, return it
        if(!is.null(inv)) 
        {
                message("getting cached inverse matrix")
                return(inv)
        }
        
        # If the inverse has not been calculated,
        # calculate and set it
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        
        # Return the inverse of the matrix x
        inv
}
