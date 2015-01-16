
#This function create object x for save and manipulate matrix
makeCacheMatrix <- function(x = matrix()) {
    #initialize variable 
    m <- NULL
    # set new matrix and reset variable
    set <- function(matrix) {
        x <<- matrix
        m <<- NULL
    }
    #get current matrix
    get <- function() x
    #get variable state
    getInverse <- function() m
    # set variable function
    setInverse <- function(inv) {
        m <<- inv
    }
    #export list of functions
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

#This function solve stored matrix
cacheSolve <- function(x, ...) {
    #get current variable state
    m <- x$getInverse()
    # if variable is not set then solve matrix and store rusult in object x
    if (is.null(m)) {
        message("Cache empty. Inverting matrix and caching result...")
        m <- solve(x$get(), ...)
        x$setInverse(m)
    }
    else {
        message("Returning cached result")
    }
    # return the cached result
    m
}
