## This code is to work out the inverse of a Matrix unless the value has 
## already been calculated and cached. In that case the saved inverse
## is restored instead of calculating it again.

## This function creates the special matrix object that can cashe its inverse(im)

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This fuction calculates the inverse of the matrix. It first checks
## to see if the value is cached and if not, it will compute it instead
## of retreiving the stored value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## check to see if it has already been calculated and cached
        if(!is.null(m)) {
                message("getting cached data!")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
