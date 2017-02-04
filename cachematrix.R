## 'makeCacheMatrix'  and 'cacheSolve' are two functions allowing to efficiently managing the retrieving on the inverse of matrix 'x'
## A new matrix can be created using the function 'makeCacheMatrix'? This function returns a list of functions indexed by :
##  - set : setting a new matrix (a new object IS NOT recreated)
##  - get : getting the current matrix
##  - setinverse : setting the inverse of the matrix. NOTE : it is highly recommanded to let the function 'cacheSolve' using 'setinverse
##                 in order to NOT recompute the invserse when the initial matrix has not changed
##  - getinverse : getting the inverse of the current matrix. NOTE : when a new matrix is set (using 'set'), the inverse is not automatically
##                 recomputed. So be sure, when using getinverse, that matrix 'x' and its inverse are synchronized

## The inverse of a matrix 'x' can be retrieved with 'cacheSolve'. This function only recomputes the inverse of 'x' when 'x' has changed.
## 'cacheSolve' also uses the 'setinverse' to set a new inverse when needed.


## Creating and managing a matrix 'x'and its inverse
## 'x' is assumed to be invertible

makeCacheMatrix <- function(x = matrix()) {
        
        ## ix : inverse of x (assuming that x is invertible)
        
        ix <- NULL ## Initialization
        
        ## This function sets the matrix to a new matrix
        ## The inverse is therfore NULL (to be recomputed)
        set <- function(y) {
                x <<- y
                ix <<- NULL 
        }
        
        ## This functions allows to return the matrix 'x'
        get <- function() x
        
        ## This function allows to set the cache the inverse of 'x'
        setinverse <- function(inverse_x) ix <<- inverse_x
        
        ## This function allows to return the inverse of 'x'
        getinverse <- function() ix
        
        ## Returning the functions as a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returning the inverse of matrix 'x'. This functions only recomputes the inverse of 'x' when 'x' changes

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## 'x' shall be a special matrix 
        ## creating with makeCacheMatrix
        
        ix <- x$getinverse()
        if (!is.null(ix)) {
                ## ix is already computed.
                ## No need to resolve the inverse
                message("getting the inverse from cache")
        } else {
                ## ix is not yet recomputed
                ## We need to solve the inverse with 'solve' function
                ## We assume that 'x' is inversible
                message("computing the inverse")
                ix <- solve(x$get(), ...)
                x$setinverse(ix)
        }
        
        ## Returning the inverse (either from cache, 
        ## either from recomputation)
        ix 
}
