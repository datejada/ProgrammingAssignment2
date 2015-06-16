## This code has two functions in order to cache the inverse of a matrix.

## Example of use:
## 1. Create a matrix call m1:
## m1 <- matrix(c(1,1,0,1),2,2) ##Example 2x2 matrix invertible
## 2. Create a object using the makeCacheMatrix function using m1 as argument:
## m1_cache <- makeCacheMatrix(m1)
## 3. Use cacheSolve function in order to get the inverse:
## cacheSolve(m1_cache)
## 4. Since this is the first time to calculate the inverse you will get:
##      [,1] [,2]
##[1,]    1   -1
##[2,]    0    1
## 5. Use againg the cacheSolve function in order to get the inverse:
## cacheSolve(m1_cache)
## 6. Since there is no change in the matrix m1, then you will get:
## getting cached data
##      [,1] [,2]
##[1,]    1   -1
##[2,]    0    1
## 7. The result is the cached value for the inverse of m1

## 1. makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Reset for the 'm' object
    m <- NULL
    ## Assigning values to 'x' and 'm' in the environment 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## creating the special "matrix" as a list containing:
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the solve
    ## get the value of the solve
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## 2. cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve function
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    ## Checking if there is a value in cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Calculating the inverse when previous value was not found
    data <- x$get()
    m <- solve(data, ...)
    ## Storing the matrix inverse calculation in cache
    x$setinv(m)
    ## Printing the result
    m  
}
