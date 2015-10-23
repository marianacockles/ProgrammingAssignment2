# Doing the calculation of the inverse of a matrix may be laborious and
# impractical. The two functions below improve the computation of this
# operation by storing previous saved values in a cache, which can be accessed
# after (when obtaining the inverse againg), instead of repeating all the
# calculation over again.

# This first function provides a "special" matrix object by creating a list
# containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        #setting the object <invs> to NULL, so that the new values can be stored
        invs <- NULL
        set <- function(y) {
                x <<- y        #setting the matrix y to the variable x
                invs <<- NULL  #reseatting invs in the parent environment
        }
        get <- function() x    #returns the matrix x
        setinverse <- function(solve) m <<- solve #set invs to the inverse of
                                                  #matrix x
        getinverse <- function() invs #returns the inverse of x previously 
                                      #cached
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse) #returns the special matrix accordingly to
                                    #the functions that were defined

}


## This second function provides the inverse of the "special" matrix created
# with the former function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse() #to check if the inverse has already been 
                               #calculated
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }                     #the function gets the information from the cache.
        data <- x$get()       #otherwise, it calculates the inverse of matrix x
        invs <- solve(data, ...)
        x$setinverse(invs)    #..and sets the inverse of x in the cache.
        invs
}
