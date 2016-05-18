## makeCacheMatrix() will create a special matrix.
## This function will take a square invertible matrix and return a list of function
## the returned list of functions are set, get, setinv and getinv
## These list of functions are input to the 2nd function cacheSolve
## cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed, 
## it'll retrieves the inverse from the cache directly.

## makeCacheMatrix() - create a special matrix and return a list.

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize inv to NULL in the local enviornment
    inv <- NULL
    ## create function set
    set <- function(y) {
        # use <<- operator to assign a value to x in the global enviornment (outside this function)
        x <<- y
        inv <<- NULL
    }
    ## function get returns value of x which is already set by matrix
    get <- function() x
    ## setinv function sets the inverse of matrix
    setinv <- function(inverse) inv <<- inverse
    ## getinv matrix will return the inverse of the matrix
    getinv <- function() inv
    ## list all the functions
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve() - will compute the inverse of a matrix and returns a matrix

cacheSolve <- function(x, ...) {
        ## input is output of makeCacheMatrix()
        ## set value of inv as the return of getinv function of the output list of 1st function
        inv <- x$getinv()
        
        ## check if the inverse is already been calculated
        if(!is.null(inv)) {
            ## get it from cache is already calculated before
            message("getting cached data")
            return(inv)
        }
        
        ## otherwise compute the inverse using solve function
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        ## sets the value of inverse in the cache via the setinv function
        x$setinv(inv)
        
        return(inv)
}
