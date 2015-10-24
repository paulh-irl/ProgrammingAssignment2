## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. 
## Note that instructions in assignment state "For this assignment, assume that the matrix supplied is always invertible." 
## So - the function does not contain any validation of this assumption.

makeCacheMatrix <- function(x = matrix()) {

	## The function makeCacheMatrix creates a special "vector", which is really a list containing a function to
	## * set the value of the vector
	## * get the value of the vector
	## * set the value of the mean
	## * get the value of the mean

	Inv <- NULL; ## Initialise
	Set <- function(y)
	{
		x << y
		Inv << NULL
	}
	Get <- function() x
	
	SetInv <- function(solve) Inv <- solve

	GetInv <- function() Inv
	
	list(set = Set, get = Get,
             setinv = SetInv,
             getinv = GetInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
        if(!is.null(m)) {
                message("Reading result from the Cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## Testing steps to follow on next line..

## - Run once and observe that it returns the inverse of the matrix.
## - Run once more and observe the same result, but this time there should also be a message stating that the result is being read from the Cache

