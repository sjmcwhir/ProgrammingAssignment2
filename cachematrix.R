## Sarah McWhirter
## Coursera -- R Programming Assignment 2
##
## cachematrix.R creates special "matrix objects" that can cache their inverse.
## makeCacheMatrix() creates this object by taking a matrix and returning a list
## of functions that allow one to set and get the matrix and its inverse.
## cacheSolve() takes one of these special matrix objects and returns the inverse,
## using the cached value if possible to save on computation resources.


## makeCacheMatrix: fn takes a matrix and returns a list of functions
## to set a new matrix, return the matrix, set the inverse of the matrix,
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize inverse
    inv <- NULL
    
    # set matrix (reinitialize inverse)
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # get matrix
    get <- function() x
    
    # set inverse
    setInv <- function(newInv){
        inv <- newInv
    }
    
    # get inverse
    getInv <- function() inv
    
    # return list of set/get functions
    list (set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve: fn takes a "cache matrix" object and returns the inverse by
## accessing the getInv() fn if the inverse is cached, and
## calculating it using solve() otherwise (if the matrix has
## changed, then its inverse value will be null)

cacheSolve <- function(x, ...) {
   
    # access x's inverse value
    inv <- x$getInv()
    
    # if cached, return cached value
    if (!isNull(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, get the matrix, calculate and recache
    # inverse, and return the new value
    m <- x$get()
    inv <- solve(m)
    x$setInv(inv)
    inv
}
