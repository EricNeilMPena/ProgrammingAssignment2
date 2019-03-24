## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## This function creates a special "matrix" object that can cache its inverse.

#Initialization of the two objects declared as x and m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #store NULL value to m and change its value later in the code
  
  #define the set function by assigning the value of y to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #define the getter for the matrix x, the value will be retrived from the parent environment
  get <- function() x
  
  #define the setter for the inverse of m
  setInverse <- function(Inverse) m <<- Inverse
  
  #define the getter for the inverse of m
  getInverse <- function() m
  
  #return a list to use the $ operator when calling on the function rather 
  #than using double brackets [[ to call the function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #retrive the inverse from an object
  #function get the inverse from the object passed in as the argument
  m <- x$getInverse()
  
  #checks to see if the result is NULL
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If the result is FALSE, cachemean() gets the matrix from the input object, 
  #calculates a solve()
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


e <- matrix(c(5,4,3,2),2,2)
e
e1 <- makeCacheMatrix(e)
e1$get()
cacheSolve(e1)
