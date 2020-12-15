### Programming assignment 2: Lexical scoping ###

## The general idea with this functions is to cache the inverse of a matrix, so that if it has
## already been calculated we don´t compute it repeatedly. Instead, we search for it in the cache

# The first function allows us to set and get the matrix, and then set and get its inverse.
# The object created by this function will have access to the get and set functions but also
# to the environment defined by the function.
# The "<<-" operator allows us to assign a value to an object from a different environment
# than the one where the function is defined.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #initializing inverse as NULL in the makeCacheMatrix environment to be used later
        set <- function(y) {
                x <<- y #assigns the value y (input) to the x object in the parent environment
                inv <<- NULL #when there's a new input this line clears prior inverse calculations
        }
        get <- function() x #get the x object from the parent environment
        setinv <- function(inverse) inv <<- inverse #assigns the input to the value of inv in the parent environment
        getinv <- function() inv #get the inverse
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv) #assign each function to a list where every element is named, so we can then use de "$" extract operator

}


# This second function needs the output of the first function as an argument and it calculates
# its inverse, but first checks if it hasn't been calculated already. We use the solve()
# function because we assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        inv <- x$getinv()  # Return a matrix that is the inverse of 'x'
        if(!is.null(inv)) { # check whether inv object is NULL
                message("getting cached data") ## if inv is not NULL we get the cached inverse
                return(inv)
        }
        matr <- x$get() #if the object inv is NULL, we get the input object (matrix)
        inv <- solve(matr, ...) # we calculate its inverse
        x$setinv(inv) #set the inverse in the input object
        inv #prints the inverse object
}


### Checking the functions ###

trial <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2)) ##creates a special matrix to be used as an argument un cacheSolve
cacheSolve(trial) ## we get the inverse of the special matrix "trial"
cacheSolve(trial) ## since we already calculated the inverse we get the cached data

## Checking that the inverse is correct
matrix <- matrix(1:4, 2, 2) ## creating a matrix
n1 <- solve(matrix) ## getting the inverse
n1 ## this matrix is the same as the one calculated in cacheSolve()
matrix %*% n1 ## we get the Identity matrix
n1 %*% matrix ## we get the Identity matrix
