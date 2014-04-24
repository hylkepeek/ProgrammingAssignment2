## makeCacheMatrix returns a set of 4 functions. It allows us to reuse a matrix wich is stored in memory (cache)
makeCacheMatrix <- function(x = matrix()) {
    ## Parameter x: The matrix  you want to inverse and store in memory

    ##Initially clear s
    s <- NULL
    
    ## set: Clear and replace the data
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get: Get the data
    get <- function() x

    ## setSolve: Store the data in memory
    setSolve <- function(solve) s <<- solve

    ## getSolve: Get the data from memory
    getSolve <- function() s

    ## Return the set of functions
    list(set = set
       , get = get
       , setSolve = setSolve
       , getSolve = getSolve
       )
}


## cacheSolve returns a matrix (which is the inverse of the actuel matrix)
cacheSolve <- function(x, ...) {
    ## Parameter x: makeCacheMatrix(x), the matrix you want to inverse and store in memory

    ## Get the cached inversed matrix. If it not exists, inverse the matrix.
    s <- x$getSolve()
    if(!is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    message("Getting uncached data")
    data <- x$get()
    s <- solve(data, ...)

    ## Cache the inversed matrix
    x$setSolve(s)
    s
}
