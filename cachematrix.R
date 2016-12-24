## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function that can create a marix and can store the inverse of that function
makeCacheMatrix <- function(x = matrix()) {
    # inverse of the matrix
    mat_inv <- NULL
    
    #create a matrix
    set <- function(y){
        x <<- y
        mat_inv <<- NULL
    }
    
    #return the original matrix
    get <- function() x
    
    #set inverse matrix
    set_inverse <- function(inv){
        mat_inv <- inv
    }
    
    #get inverse matrix
    get_inverse <- function() mat_inv
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #get the inverse of the matrix
    inv <- x$get_inverse()
    
    # if inverse is not null then return matrix
    if(!is.null(inv)){
        return(inv)
    }
    
    #get the original matrix
    data <- x$get()
    
    # inverse the matrix
    inv <- solve(data, ...)
    
    #set the inverse of the matrix
    x$set_inverse(inv)
    
    #return inverse of the matrix
    inv
}