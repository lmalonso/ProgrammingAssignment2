## This is a solution for the repeating calculation of the inverse of a matrix.
## This functions will help calculating the inverse of a given matrix, but
## with the difference that it first will look for the solution in the cached memory to 
## avoid calculating it again.

## The next function will provide information to the 'cacheSolve' function about the matrix we are working on, 
## as if the inverse was already calculated or if the matrix was changed, and store the solution in
## the cached memory.

makeCacheMatrix <- function(x = matrix()) {
     ## First, we declare variable 'inv' as NULL. This is to prepare this variable for the
     ## function cacheSolve, so it will know if the inverse should be calculated or get it from
     ## the cache memory.
     inv<-NULL
     ## We create closure 'set'
     set <- function(y) {
          ## This closure sets the value of 'x' and 'inv'. The '<<-' symbol is used so it can modify 
          ## the variables in this closure and in function 'makeCacheMatrix' 's environment
          x <<- y
          inv <<- NULL
     }
     ## We create closure 'get'
     get <- function() {
          ## When called, this closure returns the input matrix 'x'
          x
     }
     ## We create the closure 'setinv'
     setinv <- function(matinv) {
          ## When called, this closure assigns the value of the argument 'matinv' to the variable 'inv'
          inv <<- matinv
     }
     ## We create closure 'getinv'
     getinv <- function() {
          ## When called, this closure returns the variable 'inv'
          inv
     }
     ## We return a list of the closures we created
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The next function will calculate the inverse of the matrix given in 'makeCacheMatrix' function
## or will look for it if it is in the cached memory

cacheSolve <- function(x, ...) {
     ## We call 'getinv' closure from 'makeCacheMatrix' function and 
     ## assign it to the variable 'inv'
     inv<-x$getinv()
     ## The next if statement verifies if the variable 'inv' has an previous assigned 
     ## value or if it is NULL. If 'inv' it is not NULL, it means that the inverse 
     ## matrix was already calculated, so it calls the cached value of 'inv'
     if(!is.null(inv)) {
          message("getting cached data")     
          return(inv)              ## We return 'inv' as the output of 'cacheSolve' function
     }
     ## This part is to avoid getting an error message when the inverse matrix can't be 
     ## calculated
     if(det(x$get())<1.0e-10){
          print("Matrix determinant is zero. Use other matrix")
     }
     else{
          mat <- x$get()           ## This calls 'get' closure from 'makeCacheMatrix' function and assigns its value to variable 'mat'
          inv <- solve(mat, ...)   ## Using the 'solve' function, we calculate the inverse of mat
          x$setinv(inv)            ## This calls 'setinv' closure and takes the value of 'inv' to the cached memory
          inv                      ## We return 'inv' as the output of 'cacheSolve' function
     }
}
