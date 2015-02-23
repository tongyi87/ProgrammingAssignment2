## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creates all the functions into a list and returns it.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## If inverse is null, inverse is retrieved with the solve() function and assigned to the matrix using the setInverse function.
cacheSolve <- function(x, ...) {
		i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
