## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Firstly , i make a function(makeCacheMatrix) , which sets and gets the matrix and sets and gets 
#the inverse of that matrix .
#The variables x and inv are stored in the enclosing environment of the set, get, setInverse , 
#getInverse functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
#Secondly , i calculate the inverse of the matrix . 
#The get and getInverse functions only fetch x and inv from their enclosing environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
