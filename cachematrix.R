## makeCacheMatrix stores a matrix and four functions, which get and set the matrix and its inverse.
## cacheSolve checks to see if inverse of matrix is already stored, otherwise calculates it.



## Stores a matrix and list of functions to get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <-  NULL
        set <- function(y = matrix()){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <-  function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks to see if the inverse of the matrix stored in makeCacheMatrix is already calculcated, if so, return from cache
## If not calculated, calculate it and return it and save to list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
