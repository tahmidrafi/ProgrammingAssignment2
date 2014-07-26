## makeCacgeMatrix is a generator of special matrix which stores two matrices:
##      one is the original matrix
##      other is the inverse of the matrix
##
## It has four functions which can be called:
##      set(m)      --> input the given matrix and store it
##      get()       --> return the stored matrix
##      setinv(inv) --> input the inverse matrix of the given matrix and store it
##      getinv()    --> return the stored inverse matrix
##
## when called it returns a list of this four functions
## By calling each function we can manipulate the stored matrices

makeCacheMatrix <- function(x = matrix()) {
    
    ## Create the inverse matrix and set its value to NULL by default
    inv <- NULL
    
    ## Declare set function to re-initialize x
    set <- function(y) {
        x <<- y
        inv <<- NULL ## As x is reset, we have to re-calculate in inv
    }
    
    ## Get returns the matrix itself
    get <- function() x
    
    ## setinv(inv) inputs the inverse function and stores it
    setinv <- function(inv) inv <<- inv
    
    ## getinv() returns the inverse matrix
    getinv <- function() inv
    
    ## the function returns the four functions wrapped in a list
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## cacheSolve function takes input a special matrix
## created by makeCacheMatrix() function which is actually
## a list of four functions and two underlying matrices.
## 
## cacheSolve checks if the inv is already set
## if not, then it calculates the inv and sets it.

cacheSolve <- function(x, ...) {
    
    ## get the inv matrix from x
    inv <- x$getinv()
    
    ## if inv is not NULL then value of x has not changed
    ## therefor, cached data will be served
    if( !is.null(inv) ) {
        message("getting cached data")
        return(inv)
    }
    
    ## Otherwise we have to calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## store the calculated inverse for future use
    x$setinv(inv)
    
    ## return newly calculated inverse
    inv
    
}
