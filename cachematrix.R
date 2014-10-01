## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y = matrix()) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setInv <- function(inv) inv <<- inv
	getInv <- function() inv
	
	list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix`

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
        	message("getting the cached inverse data")
        	return(inv)
        }
        squareM <- x$get()
        inv <- solve(squareM)
        
        x$setInv(inv)
        
        inv
        
        
}
