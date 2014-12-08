###########################################################
## Coursera : R-Programming
## Programming Assignment 2: Lexical Scoping 
## Author: Dino N.
###########################################################

##Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #Initialize the inverse matrix of makeCacheMatrix object to 0 length Matrix
    inv <- matrix(numeric(0), 0,0) 
    #Set the matrix of makeCacheMatrix object to a new matrix and set inv to 0 length Matrix
    set <- function(y) {
        x <<- y
        inv <<- matrix(numeric(0), 0,0)
    }
    #Return the matrix of makeCacheMatrix object
    get <- function() x
    #Set the inverse of the makeCacheMatrix object to the parameter inverse
    setinverse <- function(inverse) inv <<- inverse
    #Return the inverse of the makeCacheMatrix object 
    getinverse <- function() inv
    #Make function set,get,setinverse,getinverse available
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}
#Function cacheSolve computes the inverse of the special "matrix" returned by the function makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #Get the inverse from the makeCacheMatrix object
    inv <- x$getinverse()
    #Check if makeCacheMatrix object inverse is > 0 
    if(length(inv)>0) {
        #inverse is > 0 so return it
        message("getting cached data")
        return(inv)
    }
    #inverse is = 0, inverse has to be calculated
    #get the makeCacheMatrix object matrix
    matrix <- x$get()
    #calculate the inverse matrix via solve function
    inv <- solve(matrix, ...)
    #set the inverse of makeCacheMatrix object 
    x$setinverse(inv)
    #return inverse
    inv
}