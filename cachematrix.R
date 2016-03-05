## The following pair of functions provides a way to get the inverse of 
## a square matrix without repeating the calculation each time the
## inversion matrix is needed.
#
#  Sample output:
#
#         > x<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),3,3))
#         > cacheSolve(x)
#              [,1] [,2] [,3]
#        [1,]  -24   18    5
#        [2,]   20  -15   -4
#        [3,]   -5    4    1
#         > x$getinverse()
#              [,1] [,2] [,3]
#        [1,]  -24   18    5
#        [2,]   20  -15   -4
#        [3,]   -5    4    1
#

## This function creates a special "matrix" that is a list of
## functions for the following purposes:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get=get,
          setinverse = setinverse,
          getinverse = getinverse)  
}
## The following calculates the inverse of the special square matrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the
## result in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        inv <-x$getinverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
