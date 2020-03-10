## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        ##sets inv to NULL
	set_value <- function (y)
	{
		x <<- y 
		inv <<- NULL
	}
        ## assigns values to matrix x, x being a "global" variable  
	get <- function() x 
        ## will retrive the values in x
	set_Inverse <- function(invMatrix) inv <<- invMatrix 
        ## the value in which you can set the Inverse, inv being a "global" variable
	get_Inverse <- function() inv
        ## how you can retrive the inverse

	list("set value" = set_value, "get value" = get_value, "set inverse" = set_Inverse, "get inverse" = get_Inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_Inverse()
        ## retrieving the inverse of x if there is a value
	if(!is.null(inv))
	{
		message("Retrieving cached data")
		return (inv)
	}
        ## if there already is a value, the inverse will be returned
	inv_value <- x$get_Inverse() ## gets the inverse of matrix x
	inv <- solve(inv_value) ## solves for the inverse of the matrix x
	x$set_Inverse(inv) ## sets the inverse of matrix x 
	inv ##prints inv

}
