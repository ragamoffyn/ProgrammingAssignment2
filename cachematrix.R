## This function creates and stores a list of functions
## for creating and retrieving a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## Find x in the parent environment and assign it the value in y.
        setdata <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Return the value of x, which is passed down from
        ## cacheSolve to makeCacheMatrix to getdata.
        getdata <- function() x

        ## Look for i in a parent environment, and use it to cache
        ## the inverse matrix in inv passed from cacheSolve.
        setinv <- function(inv) i <<- inv

        ## Look for i in a parent environment and return it.
        ## The value of i will be either null or, if setinv has been called,
        ## a cached inverse matrix.
        getinv <- function() i

        ## Return of list of the functions.
        list(setdata = setdata, getdata = getdata,
             setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of the matrix
## created in or retreived from makeCacheMatrix.
cacheSolve <- function(x, ...) {

        ## Create a placeholder for the inverse matrix.
        i <- NULL
        
        ## Call getinv in makeCacheMatrix to get any cached values.
        i <- x$getinv()
        
        ## If i is not null, that means there is a cached value,
        ## so just return that and then quit.
        if(!is.null(i)) {
                print("getting cached data")
                return(i)
        }

        ## Otherwise, if i is null, then there are no cached values.
        ## In that case, call getdata in makeCacheMatrix to assign
        ## any matrix values currently in x to data.
        data <- x$getdata()
        
        ## Calculate the inverse of the matrix in data and assign it to i.
        i <- solve(data)

        ## Cache the inverse matrix now stored in i by calling
        ## setinv in makeCacheMatrix.
        x$setinv(i)

        ## Return the inverse matrix.
        i
}
