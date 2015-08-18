# A pair of functions to 1) compute the inverse of a matrix if it 
# hasn't already been cached, pulling it from the cache if it has and
# 2) handle the caching of a matrix.
# Doug Jackson
# Coursera R course

# Make a list which is sort of a bastardized object with "methods"
# to get and set the value of a matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

# Obtain the inverse of a matrix, either by pulling it from the cache
# or calculating it if it's not already in the cache
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of x
    m <- x$getmean()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
