##  ******************************************************
##	Author: Jaro Vomacka
##	Assignment: Programming Assignment 2: Lexical Scoping (Week 3)
##	Course:	R Programming by Johns Hopkins University
##
##  ******************************************************
##	Overall, this assignment highlights the power and intricases of lexical scoping.
##	This R code takes advantage of lexical scoping.
##	The power of the code comes in because functions can return objects of type list(),
##	and this list() of functions allows access to any other objects defined in the environment of the original function.
##	Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix,
##	rather than compute it repeatedly.
##	These functions cache the inverse of a matrix.
##	In the makeCacheMatrix() function, the subsequent code can access specific values,
##	through the use of getters (program modules that retrieve data) and setters (program modules that sets data).
##	The function cacheSolve() is then able to calculate or retrieve the inverse of the matrix.
##	Retrieve if currently cached (hence saving computing time and power).
##	Computing the inverse of a square matrix is done with the solve function in R.
##	For this code, it is assumed that the matrix supplied is always invertible.
##
##  ******************************************************
##
##	makeCacheMatrix:
##	This function creates a special matrix object that can cache its inverse.
##	This function builds a set of functions and returns the functions within a list to the parent environment.
##	Please note that the "<<" (double left arrow) indicates that the assignment should be made to the parent environment

makeCacheMatrix <- function(x = matrix()) { 	##initialise x as matrix form

		m <- NULL 								##initialise m as NULL
        set <- function(y) {						
                x <<- y							##Assign the input argument to the x object in the parent environment
                m <<- NULL						##Assign the value of NULL to the m object in the parent environment
        }
        get <- function() x						##defines the getter for the matrix x
												##R retrieves x from the parent environment
        setinv <- function(solve) m <<- solve	##defines the setter for the inverse m
        getinv <- function() m					##defines the getter for the inverse m
		
##	The getters and setters are now defined for both of the data objects within the makeCacheMatrix() object.		
		
        list(set = set,							##assigns each of these functions as an element within a list(),
			 get = get,							##and returns it to the parent environment
             setinv = setinv,					##each element in the list is defined with a name,
             getinv = getinv)					##this allows us to use the $ extract operator to access the functions by name	 

}

##	cacheSolve:
##	This function computes the inverse of the matrix returned by makeCacheMatrix above.
##	If the inverse has already been calculated (and the matrix has not changed),
##	then the cachesolve should retrieve the inverse from the cache.
##	Please note that the passing variable must be of the type makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinv()							##function attempts to retrieve an inverse matrix from the object passed in
        if(!is.null(m)) {						##checks to see whether the result is NULL
                message("getting cached data")	##if the value here is not equal to NULL, there exists a valid, cached inverse matrix, 
                return(m)						##and can return it to the parent environment
        }

		##If the result of !is.null(m) is FALSE, the code below executed.		
		
        data <- x$get()							##cacheSolve() gets the matrix from the input object, 
        m <- solve(data, ...)					##calculates the inverse,
        x$setinv(m)								##uses the setinv() function on the input object to set the inverse,
        m										##and then returns the value of the inverse to the parent environment
}