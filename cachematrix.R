######################################################################
## Coursera   : R Programming Course
## Date       : April 2014
## Assignment : 2
## Due        : 27/APR/2014
## Notes      : This file contains 2 functions which work together
##              to solve and cache the result of matrix inversion.
######################################################################
#  Functions
#  =========
#  (1) `makeCacheMatrix`: This function creates a special "matrix"
#       object that can cache its inverse.

#  (2) `cacheSolve`: This function computes the inverse of the
#       special "matrix" returned by `makeCacheMatrix` above. If 
#       the inverse has already been calculated (and the matrix 
#       has not changed), then `cacheSolve` will retrieve the
#       inverse from the cache.
#####################################################################
# Usage
# =====
#  makeCacheMatrix
#  ---------------
#    
#     m <- c(3,0,2,2,0,-2,0,1,1)    # Create vector m
#     dim(m)<-c(3,3)                # Convert m to matrix
#     a <- makeCacheMatrix(m)       # Call makeCacheMatrix
#     a$get()                       # Display matrix m
#              [,1] [,2] [,3]
#       [1,]    3    2    0
#       [2,]    0    0    1
#       [3,]    2   -2    1
#    a$getInverse()                 # get inverse of m
#       NULL                        # Not set yet.
#
#  cacheSolve
#  ----------
#    >b <- cacheSolve(a)            # "a" created above
#    >b                             # display result
#            [,1] [,2] [,3]
#      [1,]  0.2 -0.2  0.2
#      [2,]  0.2  0.3 -0.3
#      [3,]  0.0  1.0  0.0
#   >b<-cacheSolve(a)               # solve already solved inverse
#       getting cached data
#   >a$getInverse()                 # get inverse
#            [,1] [,2] [,3]
#      [1,]  0.2 -0.2  0.2
#      [2,]  0.2  0.3 -0.3
#      [3,]  0.0  1.0  0.0
#   >a$set(m)                      # set a new matrix
#   >a$getInverse()                # get inverse
#     NULL                         # inverse is now NULL 
#
#####################################################################
# Limitations
# ===========
# (1) The matrix supplied to the function must be invertable.
######################################################################


#=====================================================================
# "makeCacheMatrix" is a function with the following properties:
#     1. it takes an argument x of type matrix
#     2. it returns a list with 4 list items  (they are four functions
#        wrapped in a list)
#        a) set
#        b) get
#        c) setInverse
#        d) getInverse
#=====================================================================
makeCacheMatrix <- function(x = matrix()) {
            matrix_inverse <- NULL      
            set <- function(y) {
                    x <<- y
                    matrix_inverse <<- NULL
            }
            get <- function() x                  
            setInverse <- function(solve) matrix_inverse <<- solve
            getInverse <- function() matrix_inverse
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


#=====================================================================
# "cacheSolve" is a function that will return the inverse of matrix.
#  If the inverse has been previously calculated then the cached 
#  value is returned.
#  NB: Input into this function is from makeCacheMatrix
#      See usage examples above.
#=====================================================================

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            matrix_inverse <- x$getInverse()       #query x vector's cache       
            if(!is.null(matrix_inverse)) {         #Cache is Empty?
                    message("getting cached data") #Not empty,
                    return(matrix_inverse)         #Return cached value
            }
            data <- x$get()                        #Get orig matrix       
            matrix_inverse  <- solve(data, ...)    #Calc inverse
            x$setInverse(matrix_inverse )          #Cache inverse
            matrix_inverse                         #Return inverse
}
