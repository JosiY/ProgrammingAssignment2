# This file contains two functions:
#
#   makeCacheMatrix     for creating a cached matrix
#   cacheSolve          for returning the inverse
#  


# makeCacheMatrix(A)
# This function creates a list of scoped functions for storing 
# a matrix and its inverse.
#
# Inputs:
#  A            a matrix
#
#
# Outputs:
# a list of scoped functions 
#
#
makeCacheMatrix <- function(A = matrix()) {
        # returns a list of scoped functions 
        
        A_inv <- NULL            # intitialize inverse as NULL
        
        set <- function(newA) {
        # Set the numeric matrix stored in the chached matrix
                A <<- newA      # set new matrix
                A_inv <<- NULL  # clear its cached inverse
                         }
        get <- function() { 
        # get the numeric matrix stored in the chached matrix 
                A               # return matrix
        } 

        setinverse <- function(newInverse) {
        # set the value of inverse        
                A_inv <<- newInverse
        }
        
        getinverse <- function() {
        # get the value of inverse 
                A_inv
        }
        
        # return a list containing the scoped functions for this instance of makeCacheMatrix 
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}
 

# cacheSolve(A) computes the inverse of a matrix depending on the numbers of inputs.
# If the inverse has been computed, already, the cached inverse is returned
#
cacheSolve <- function(cachedMatrix, ...) {
        # returns the inverse of cachedMatrix
        
        A_inv <- cachedMatrix$getinverse()  # get content of "storage" for inverse marix
        
        # Iff A_inv has been initialized with a valid cached inverse, return it directly
        if(!is.null(A_inv)) {
                message("getting cached data")
                return(A_inv)
        }
        
        # Otherwise ... 
        A <- cachedMatrix$get()         # get the numeric content of the chached matrix structure (the matrix itself)
        tmp <- solve(A, ...)            # temporally store the inverse in a variable tmp
        cachedMatrix$setinverse(tmp)    # store the tmp as new cached inverse
        tmp                             # return the inverse (which is still cached in tmp)
}


