# A pair of functions to 1) compute the inverse of a matrix if it 
# hasn't already been cached, pulling it from the cache if it has and
# 2) handle the caching of a matrix.
# Doug Jackson
# Coursera R course

# Make a list which is sort of a bastardized object with "methods"
# to get and set the value of a matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    
    # Set the data and clear invMat
    set <- function(m) {
        mat <<- m
        invMat <<- NULL
    }
    set(x)
    
    get <- function() {return(mat)}
    
    setInv <- function(iM) {invMat <<- iM}
    
    getInv <- function() {return(invMat)}
    
    return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}

# Obtain the inverse of a matrix, either by pulling it from the cache
# or calculating it if it's not already in the cache
cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of x
    m <- x$getInv()
    
    # Pull the inverse from cache if it's there
    if(!is.null(m)) {
        cat("Getting cached data.\n")
        return(m)
    }
    
    # Otherwise, calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    return(m)
}
