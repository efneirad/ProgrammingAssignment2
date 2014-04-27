## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##creation of special matrix object
	inv <- NULL   ##inicialization of an inverse matrix
	set <- function(y) {  ##function of set matrix's values. If the matrix is changed, then its inverse is initialized again 
		x <<- y
		inv <<- NULL   
	}
	get <- function() x	 ##return matrix
	setinv <- function(inverse) inv <<- inverse  ##set inverse matrix function
	getinv <- function() inv ##return inverse of matrix, if said inverse hasn't been calculated yet then return null
	list(set = set,get = get, ##list of functions
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(x) { ##function that calcules and returns the inverse of special matrix 'x'
	inv <- x$getinv() ## first, it gets the inverse of the matrix 'x'
        if(!is.null(inv)) { ## if the inverse of the matrix has already been calculated then returns it
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ##if not, then get data of x 
        inv <- solve(data) ##calcule inverse of acquired data
        x$setinv(inv) ##set the inverse of object x
        inv	##return the inverse matrix
}
