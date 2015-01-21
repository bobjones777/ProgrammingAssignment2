
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. This assignment is to write a pair of functions that
## cache the inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse
## The input matrix is initially an empty matrix

makeCacheMatrix <- function(x = matrix())
{
        ## Initializing the inverse matrix
        inv <- NULL
        
        ## Set the matrix
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the matrix
        get <- function()
        {
                x
        }
        
        ## Set the inverse matrix
        setinv <- function(inverse)
        {
                inv <<- inverse
        }
        
        ## Get the inverse matrix
        getinv <- function()
        {
                inv
        }
        
        ## Return a list including all inner functions above
        invisible(list(set = set, get = get,
                       setinv = setinv,
                       getinv = getinv))
}


## Computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...)
{
        ## Get the inverse of matrix x from the cache
        inv <- x$getinv()
        
        ## If the inverse has already been calculated, return it
        if(!is.null(inv)) 
        {
                message("getting cached inverse matrix")
                return(inv)
        }
        
        ## If the inverse has not been calculated,
        ## calculate and set it
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        
        ## Return the inverse of the matrix x
        inv
}
