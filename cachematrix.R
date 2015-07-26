## Calling makeCacheMatrix creates an object that internally has 
## 4 functions (methods). function(x=matrix()) is like a 
## constructor in that calling it sets the member variable x
## to the value that was passed in.
##
## This is why a separate call to set is not required.
## When you call makeCacheMatrix() and pass it a matrix, it
## sets the member variable x.

## Construct a matric object that caches a matrix and
## it's inverse
##
makeCacheMatrix <- function(x = matrix()) {
    # init the cached mean to NULL
    # i is member variable for the class
    i <- NULL
    
    # set the internal member x to the value passed
    # in through y. NULL the cached mean since the
    # vector has now changed and the mean has not been computed
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Return the vector
    get <- function() x
    
    # Set the member variable m to the value
    # passed in through the mean parameter
    # Calls to setinverse() should be performed
    # only by code that is actually computing
    # the inverse
    setinverse <- function(inverse) i <<- inverse
    
    # Get value cached in the member
    # variable i and return it to the caller
    getinverse <- function() i
    
    # Build a list of function pointers to the
    # functions defined above.  The function name
    # "makeCacheMatrix" acts like a 'this' pointer
    # and the function names defined here can be
    # access as follows:
    #   myMatrix <- makeCacheMatrix(someMatrix)
    #   temp <- myMatrix$get
    #   myMatrix$set(differentMatrix)
    #   inverseMatrix <- myMatrix$getinverse()
    #
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse matrix for makeCacheMatrix objects.
## x is an object created by the makeCacheMatrix function.
## Example usage:
##   myMatrix <- makeCacheMatrix(someMatrix)
##   myInverse<-cacheSolve(myMatix)
##   # calling a 2nd time produces the cached data message
##   myInverse<-cacheSolve(myMatix)
##   getting cached data
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
