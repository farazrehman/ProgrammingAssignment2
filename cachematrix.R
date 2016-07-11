## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    makeCacheMatrix <- function(x = matrix()) {
        ## Assuming per assignment description that x is coming in as an square 
        ##      invertable matrix
        ##  List of functions:
        ##  -set matrix
        ##  -get matrix
        ##  -set inverse
        ##  -get inverse
        ##        
        
        inv = NULL
        set = function(y) {
            # use `<<-` to assign a value to an object in an environment 
            # different from the current environment. 
            x <<- y
            inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
    }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Compute inverse of the original matrix using function: makeCacheMatrix
    
    inv = x$getinv()
    
    # If already inverse is there get if from cache
    if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("Getting from Cache")
        return(inv)
    }
    
    # If not in cache, calculate using the function
    matrix.data = x$get()
    inv = solve(matrix.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}
