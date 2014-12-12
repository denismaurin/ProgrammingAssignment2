## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix holds a list of getters and setters to set/get the matrix and the inverse
## and an internal variable inv which is accessed through these setters/getters
## If the inverse is available e.g. was computed, then the getinverse function returns the variable
## otherwise it returns its default value NULL
## The cache function attempts to retrieve the inverse. If the result is NULL (not previously calculated)
## then the solve function is involked and the result stored in makeCacheMatrix instance
## Next time the getinverse() function fetches directly the precomputed inverse and returns
# directly the result

## Write a short comment describing this function
## Input: a matrix (assumed inversible)
## Internal variable: inv storing the precomputed inverse
## get() function: get the input matrix
## set() function: set the input matrix
## getinverse() function: returns the inverse (inv variable)
## setinverse() function: set the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve() function takes a makeCacheMatrix as input
## and queries makeCacheMatrix to check if an inverse matrix is already available
## if so returns this matrix directly, if not calculate the inverse and store/cache it
## for next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
