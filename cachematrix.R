## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix" is a function that defines variables and functions, then returns a list of the defined functions. the purpose of this function is to take advantage of scoping to store the inverse of a function. To further elaborate, the list that is returned is a data structure that will hold a matrix, a inverse, and allow some ops on said matrix and inverse, namely storing them and getting them
## | myMatrix        |
## -------------------
## | x, passed       |
## | inverse, private|
## | set             |
## | get             |
## | setInverse      |
## | getInverse      |
## -------------------
## "x" is matrix
## "inverse" is (cacheable) inverse of matrix
## "set" is a function that sets the matrix, x, in the parent's env,  to a provided matrix, and set the parent inverse to NULL
## "get" is a function that returns x
## "setInverse" function to set inverse from a passed variable
## "getInverse" <- function to get inverse
## 


makeCacheMatrix <- function(x = matrix()) {
        inverse<- NULL
        set <- function(y) {
                x <<- y
                inverse<<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse<<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Assumption(s): - the matrix supplied is always invertible.
##                - user is responsible to set rm.na   
##                - This function will not type/class check x 
## cacheSolve: checks if cache (inverse of 'x') exists. If so, returns cache; otherwise, computes inverse of 'x' and stores it as cache
## x is a special matrix constructed by the above function, 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
        ##check if cache of inverse exists. If so, return it
        if (!is.null(x$getInverse())){
                message("getting cached data")
                return(x$getInverse())
        }
        #otherwise, compute and store it, then return it
        else{
                mymat <- x$get()
                inverse <- x$setInverse(solve(mymat),...)
                return(inverse)
        }
        ## Return a matrix that is the inverse of 'x'
}
