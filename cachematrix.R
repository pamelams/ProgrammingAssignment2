## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of the matrix or return the the inverse from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
