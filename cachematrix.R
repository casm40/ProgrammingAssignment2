## Calculation of the inverse of a matrix can be computationally intensive, particularly if loops are involved.
## This pair of functions create a special object that stores a matrix-related list that caches its' inverse.

## The function makeCacheMatrix takes a matrix and creates a list that caches its inverse. 
## It contains four functions to get and set matrix data and to get and set the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The function cacheSolve retrieves a previously cached value, if available. 
## If not cached, it calculates the inverse of the matrix returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
