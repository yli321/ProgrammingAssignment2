## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    matrix_inv <- NULL
    set <- function(y) {
        x <<- y
        #initialize matrix_inv to NULL
        matrix_inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inv) matrix_inv <<- inv
    
    getInv <- function() matrix_inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

# cacheSolve returns the inverse of the matrix
# If the inverse has already been calculated and the matrix has not changed,
# then it will return the result and skip the computation.
# If not, it does the computation, sets the value in cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix_inv <- x$getInv()
    if(!is.null(matrix_inv)) {
        message("getting cached data")
        return(matrix_inv)
    }
    
    data <- x$get()
    matrix_inv <- solve(data)
    x$setInv(matrix_inv)
    matrix_inv
}
