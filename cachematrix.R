##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
	i <- NULL
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
	get <- function() x
	setInv <- function(inv) i <<- inv
	getInv <- function() i
	list(get = get, getInv = getInv, set = set, setInv = setInv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
	i <- x$getInv()
	if(!is.null(i)) {
		message("getting cached matrix")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix, ...)
	x$setInv(i)
	i
}