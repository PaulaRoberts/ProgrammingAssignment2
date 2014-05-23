# These functions are based on those given in the Coursera R Programming Assignment 2 specifications. 
# They allow the inverse of a matrix to be calculated and then stored (cached) so that the value 
# can be retrieved later without redoing the calculation.


# The makeCacheMatrix function creates an R object of type 'list'. 
# This list contains 4 elements, set, get, setmean and getmean. 
# Each of these elements are in fact functions which set a vector 
# value, get (or return) that vector value, set (or calculate) the 
# mean of the vector and get (or return) the mean value respectively. 
# This function uses nested functions and the <<- operator which 
# allows values to be assigned to an object in a different environment 
# to the current one. This is Lexical Scoping.

makeCacheMatrix <- function(x = numeric()) 
{
        m <- NULL					# initialising the variable m to NULL
        
        set <- function(y) 
        {				        	# nested function
                x <<- y					# x (defined outside this function) assigned the local value of y
                m <<- NULL				# m (defined outside this function) assigned NULL
        }
        
        get <- function() x				# assigns the function get to return the value of x
        
        setInverse <- function(solve) m <<- solve	# assigns the function setInverse to calculate the inverse of m
        
        getInverse <- function() m			# assigns the function getInverse to return the inverse of m


        
	list(set = set, get = get,			# autoprints the list of functions created by this function. 					
             setInverse = setInverse,
             getInverse = getInverse)			# As this is the last line, this is the return value of the makeCacheMatrix function.

}




# The cacheSolve function uses the data stored in the makeCacheMatrix function.
# That is, this function calls the required functions from the list 
# of functions returned by makeCacheMatrix.
# Specifically, it calls the getInverse() function to retrieve the 
# inverse of the matrix stored by makeCacheMatrix. 
# If this value has not already been calculated, then another 
# makeCacheMatrix function, namely setInverse() is called

cacheSolve <- function(x, ...)  	# the ... argument is used to indicate further paramaters
{
        m <- x$getInverse()     	# m is assigned the return value of the getInverse() function
        if(!is.null(m))         	# check to see if this value is NOT NULL (which is the inital value assigned to this variable in makeCacheMatrix)
        {
                message("getting cached data")
                return(m)       	# returns the result of getInverse and exits from this function
        }
        data <- x$get()         	# if the value of m is NULL, then the data (that is, the matrix stored in makeCacheMatrix) is retrieved
        m <- solve(data, ...)   	# the inverse of the matrix is calculate and assigned to m
        x$setInverse(m)         	# this value is set in makeCacheMatrix so it can be retrieved later if required
        m                       	# the value of m (now an inverse matrix) is autoprinted
}
