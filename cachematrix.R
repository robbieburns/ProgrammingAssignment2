## Functions to create a matrix that can cache it's inverse 
## (makeCacheMatrix), and a function that can return the inverse 
## of a matrix (solve), using the cached copy of the inverse if it 
## exists (cacheSolve).

## makeCacheMatrix: Construct a list object that can return the matrix that was 
## initially supplied, and can cache a copy of the inverse (which
## can also be returned as neede)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: Caching version of "solve", uses the inverse-caching
## list object supplied by makeCacheMatrix so that the
## inverse only needs to be calculated once
##
## Additional arguments to "solve" can be provided to this 
## function by way of (...)

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    ## Return a matrix that is the inverse of 'x'
    x$setInv(inv)
    inv
}
