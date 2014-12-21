## These functions allow for the caching of a matrix and
## its inverse.  They were copied from the Example in the
## Programming Assignment and modified to calculate the
## matrix inverse instead of the mean.

## The makeCacheMatrix function creates a list that contains 
## four functions associated with a cached "matrix" object:
##  "set" - used to modify the cached matrix 
##          object and clear the cached inverse 
##  "get" - used to return the cached matrix
##  "setinv" - used to set the inverse cache value
##  "getinv" - used to return the cached inverse 

makeCacheMatrix <- function(x = matrix()) {
        ## Returns list of functions

        ## Initialize the inverse cache 
        m <- NULL

        ## function code to change the cached input matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## function code to return the cached input matrix 
        get <- function() x

        ## function code to set the cached inverse matrix
        setinv <- function(inv) m <<- inv

        ## function code to return the cached inverse matrix 
        getinv <- function() m

        ## return a list of the four component functions 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The cacheSolve function takes a makeCacheMatrix object 
## and tests to see if the inverse has already been cached.
## If it has already been cached, the function returns the 
## cached inverse.  If it has not already been cached, then
## the function computes, caches, and returns the inveerse

cacheSolve <- function(x, ...) {
        ## Return a matrix m that is the inverse of 'x'

        ## Check for cached inverse
        m <- x$getinv()

        ## Return cached inverse, if it exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## If cached inverse does not exist, get the matrix
        data <- x$get()

        ## Calculate the inverse
        m <- solve(data, ...)

        ## Cache the inverse
        x$setinv(m)

        ## Return the inverse
        m
}
