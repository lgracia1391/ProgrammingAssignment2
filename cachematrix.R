## Computing the inverse of a square matrix taking advantage of cache previous computation.
## Assignment uses solve(X) to calculate the inverse of the matrix. 
## It also assumes that the matrix supplied is always invertible.
## It includes funtions: makeCacheMatrix to create a "special" matrix out of the original one, and
## cacheSolve to calculate the inverse of the original matrix using cache functionality based on scoping

## makeCacheMatrix takes a invertible matrix as input and outputs four funtions in a list to allow
## defining and getting the matrix plus preparing the calculation with solve(X) or returning its cached value if existing

makeCacheMatrix <- function(x = matrix()) {
        ## Denfines a "special" matrix and prepares it to get its inverse from cache it already calculated
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## cacheSolve returns the inversed of an invertible matrix defined with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mymat <- x$get()
        m <- solve(mymat, ...)
        x$setsolve(m)
        m
}
