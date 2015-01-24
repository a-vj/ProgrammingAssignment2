## This code has a function to define a special matrix object that can
## cache its inverse value and a solver method to fetch the inverse value
## from the cache (if not present it will calculate the inverse value and
## stores in cache)

## Function   : makeCacheMatrix
## Description: This fuction creates a special "Matrix" object
##              that caches its inverse once computed. When the values
##              are changed, cache is cleared. This special matrix
##              object has four methods associated with it:
##
##              1. get()    : returns the value of the matrix
##              2. set(x)   : sets the matrix value as specified in x
##              3. getinv() : returns the cached inverse value of this matrix
##              4. setinv(i): caches the inverse value this matrix
##
## Usage e.g  : 1. To initialize the object:
##                 a <- makeCacheMatrix(x*)
##                      x -> an optional regular matrix object
##              2. To set a new value
##                 a$set(x)

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         
         set <- function(y) {		# assigns a new value to matrix and
                x <<- y				# clears cache
                inv <<- NULL
         }
         
         get <- function() x		# returns value of matrix
         
         setinv <- function(inv_matrix) inv <<- inv_matrix
									# stores the computed inverse value in cache
         
         getinv <- function() inv	# returns cached inverse value
         
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## Function   : cacheSolve
## Description: Returns a matrix that is the inverse of 'x'.
##              This function first tries to fetch the inverse value of matrix
##              from cache of special matrix object defined above. If cached
##              value is not available it calculates the inverse and stores in cache
## Usage e.g  : cacheSolve(x)
##              x - special matrix object

cacheSolve <- function(x, ...) {
        inv <- x$getinv()					## checks if inverse is already
        if(!is.null(inv)) {					## available in the cache memory
            message("getting cached data")
            return(inv)
        }
        
        data <- x$get()						## if not available in cache
        inv <- solve(data, ...)				## inverse is computed and cached
        x$setinv(inv)						## for future use
        
        inv									## returns the inverse matrix
}