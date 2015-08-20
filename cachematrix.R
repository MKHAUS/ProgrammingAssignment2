## cacheSolve gets the Matrix inverse from caache, if available, 
## or otherwise solves directly with use of functions defined in 
## makeCacheMatrix

##-----------------------------------------------------------------

## makeCacheMatrix
## Inputs: x (Matrix)
## Returns list of functions: set, get, setinv, getinv
##
## set - sets x to value passed in
## get - returns x value
## setinv - sets inv to inverse value passed in
## getinv - returns inv value
##
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
      	x <<- y
      	inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, 
		get = get,
		setinv = setinv,
		getinv = getinv
	)
}


## cacheSolve
## Inputs: x (Matrix)
## Returns the Matrix Inverse of x
##
## Check to see if the inverse has already been calculated
## If so return from Cache else calculate the matrix inverse
##
cacheSolve <- function(x, ...) {

	inv <- x$getinv()
	## check for matrix inverse in cache
	if(!is.null(inv)){
		message("getting cached data")
            return(inv)
	} 
	## calculate matrix inverse
	data <- x$get()
	inv <- solve(data, ...)
	## set matrix inverse to cache
	x$setinv(inv)
	inv
}
