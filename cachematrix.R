# The makeCacheMatrix function is a set of 4 functions that is able to cache a matrix 
# by using the scoping rules of R to resolve free variables. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {         # The set function uses the <<- operator to cache a given matrix as the variable x. 
                x <<- y              # x and s are defined outside of the functions definition enviroment. s is set to NULL,
                s <<- NULL           # which requires s to be calculated.
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,   # The list function returns an object that contains the 4 internal functions and 
             setsolve = setsolve,    # can be used by the subsequent cacheSolve function.
             getsolve = getsolve)
}

# By calling the cacheSolve function on the object returned by the makeCacheMatrix funtion, the initialized matrix is
# inverted using the solve () function. The inverted matrix is only calculated if the variable s is empty. Otherwise 
# the inverted matrix (assigned to the variable s) is returned from the cache without re-calculation by elegant use of
# lexical scoping.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {                               # when s is NULL, the if statement is not executed and s is
                message("getting cached data")          # calculated below (lines 29-30)
                return(s)
        }
        data <- x$get()                                 
        s <- solve(data, ...)
        x$setsolve(s)                                   
        s                                              
}                                                      