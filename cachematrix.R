## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix is a function that allows its user to store a matrix 
## and its inverse along with internal functions that enable the user
## the user to manipulate its cached contents: the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the variable inv to hold the inverse of the matrix x
        inv <- NULL
        
        ## The set function allows the user to reset the items stored in the 
        ## cached matrix back to its default setting
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## The get function allows the user to retrieve the stored matrix 
        get <- function() x
        
        ## The setinv function allows the user to set a matrix called x_inverse
        ## as the inverse of the current cached matrix x
        setinv <- function(x_inverse) inv <<- x_inverse
        
        ## The getinv function allows the user to retrieve the stored cached 
        ## inverse matrix stored in the variable inv
        getinv <- function() inv
        
        ## Returns a list containing the cached matrix and its inverse 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## cacheSolve is a function that allows a user to find the inverse of 
## a matrix x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Note: x is a cached matrix which is a list
        
        ## using the getinv function in x to retrieve the cached inverse
        inv <- x$getinv()
        
        
        ## This conditional statment checks if the returned inverse is 
        ## NULL or not.  If it is not NULL, then a message will be printed
        ## to notify the user of the inverse and the inverse of x is returned
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If inv is NULL, then the cached matrix x is retrieved using the get
        ## function in x
        cacheMatrix <- x$get()
        
        ## Using the solve function in R, calculate the inverse of cacheMat
        inv <- solve(cacheMat, ...)
        
        ## Using the setinv function in x to store the inverse of cacheMat in x
        x$setinv(inv)
        
        ## returns the inverse of the matrix x
        inv
}
