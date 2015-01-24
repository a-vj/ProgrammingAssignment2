# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# -------------------------------------------------------------------------------

# Function    : makeCacheMatrix
# Description : This fuction creates a special "Matrix" object that can 
#              	cache its inverse. When the values are changed, cache is cleared.
#              	This function returns a list of functions to do the following:

# 1. Set the value of the matrix - set(x) : sets the matrix value as specified in x
# 2. Get the value of the matrix - get() 

# 3. Set the value of the inverse matrix - setinverse(i): caches the inverse value of this matrix
# 4. Get the value of the inverse matrix - getinverse()

makeCacheMatrix <- function(x = matrix()) {
		# inv - Store the cached inverse matrix
        inv <- NULL
        
        # Setter function to Set matrix Value
        # and clear the cache (inv variable)
        set <- function(y) {
        	x <<- y
        	inv <<- NULL
        }

        # Getter function to Get matrix Value
        get <- function() x
        
        # Setter function to store the computer Inverse matrix Value
        setinverse <- function(inverse_matrix) inv <<- inverse_matrix
		
        # Getter function to Get the cached inverse value
        getinverse <- function() inv	
        
        # Return the matrix with all four defined functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



# Function    : cacheSolve
# Description : This fuction returns a matrix that is the inverse of 'x'.
#              	It will try to get the value from Cache. If not available,
#				It will compute the inverse and store it in cache via setinverse function.

# Note 		  : This function assumes that the matrix is always invertible.

cacheSolve <- function(x) {

	# Get the inverse
    inv <- x$getinverse()

    # Check if inverse is available (If not NULL, then its available)
    if(!is.null(inv)) {
        message("Cached data available. Getting Cached Data ... Done.")
        #Return the Cached Inverse Matrix
        return(inv)
    } else {

        message("No Cached data available. Computing Inverse Matrix...")
        # Get the matrix from Object X
	    data <- x$get()
	    # Find the Inverse Matrix
	    inv <- solve(data)
	    # Assign the Inverse Matrix for cache
	    x$setinverse(inv)
	    message("Computing Completed.")
	    #Return the Computed Inverse Matrix
	    inv
    }

}
