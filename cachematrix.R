## makeCacheMatrix() and cacheSolve() functions in conjunction:
        # gets the inverse of the matrix from cache, or
        # solves the inverse otherwise


## makeCacheMatrix() makes a special matrix, containing list of 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y = matrix()) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(inv = matrix()) s <<- inv
        
        getsolve <- function() s
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
 
}


## cacheSolve() pulls the cached inverse of Matrix, or solves it otherwise
cacheSolve <- function(x, ...) {
        
        s <- x$getsolve()
        
        if(!is.null(s)) {
                message("Getting cached Matrix Inverse")
                return(s)
        }
        
        dat <- x$get()
        s <- solve(dat, ...)
        x$setsolve(s)
        
        s
                
}
