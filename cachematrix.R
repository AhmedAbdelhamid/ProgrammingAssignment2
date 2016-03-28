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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	Inv <- x$getInv()  
	if(!is.null(Inv)) {
		return(Inv)
	}
	data <- x$get()
	Inv <- solve(data)
	x$setInv(Inv)
	Inv
}
