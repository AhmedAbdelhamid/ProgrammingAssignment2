## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function make a special matrix and returns a list containing a functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
	x <<- y
	m <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function calculate the inverse of the special matrix created with makeCachMatrix() function
## It first checks to see if the inverse has already been calculated. If so, it  gets the inverse from the cache
## Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache via the setInv function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	Inv <- x$getInv()  
	if(!is.null(Inv)) {
		return(Inv)
	}
	data <- x$get()
	Inv <- solve(data)
	x$setInv(Inv)
	Inv
}
