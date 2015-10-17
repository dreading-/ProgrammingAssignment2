## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These functions create a  matrix cache that stores the matrix and its inverses.
##
## For a given matrix, makeCacheMatrix initialises the cache and cacheSolve 
## returns the matrix's inverse. The initial call to cacheSolve will compute the inverse. 
## Subsequent calls to cacheSolve will return a cached inverse
## 
## Example usage:
##      m = makeCacheMatrix(x)  ## initialise cache
##      cacheSolve(m)           ## return inverse of x
## 
## Assumptions:
##      The matrix supplied is always invertible.

## Initialise the matrix cache
makeCacheMatrix <- function(data = matrix()) {
        inverse <- NULL
        set <- function(y) {
                data <<- y
                inverse <<- NULL
        }
        get <- function() data
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Fetch inverse from cache. If it does not exist in cache then it computes the inverse
cacheSolve <- function(matrixCache, ...) {
       
        inverse <- matrixCache$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse.")
                return(inverse)
        }
        data <- matrixCache$get()
        inverse <- solve(data)
        matrixCache$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}

## Test Code
##> x = rbind(c(1, 2), c(3, 4))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    2
##[2,]    3    4
##> cacheSolve(m)
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> cacheSolve(m)
##getting cached inverse.
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> 
