## The follwoing R functions are able to cache potentially time-consuming computations. 
## For example, taking the inverse of a matrix is typically a fast operation. 
## However, for a very large matrix, it may take too long to compute the inverse, 
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 
## In the following R code will take advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object.

## The "makeCacheMatrix" function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- inverse(data, ...)
    x$setinverse(i)
    i
}
