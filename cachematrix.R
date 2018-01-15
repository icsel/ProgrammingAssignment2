## These functions allow us to avoid the costly, repeated 
## calculations of matrices inverses. It does so by caching 
## the inverse of a given matrix, and when an inverse is called,
## it checks whether it has already been calculated.


## makeCacheMatrix() creates an R object that stores a matrix 
## and its inverse.

## It builds a set of functions (4 in total), and it returns 
## these functions within a list to the parent environment. 
## Besides the four functions, it includes two data objects, 
## 'x' and 'i'.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL ## initialize objects
      set <- function(y) { ## define functions
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list (set = set, get = get, ## return a list
            setinverse = setinverse, 
            getinverse = getinverse)
}

## cacheSolve() requires an argument returned by 
## makeCacheMatrix() in order to retrieve the inverse from 
## the cached value that is stored in the MakeCacheMatrix() 
## object's environment.

cacheSolve <- function(x, ...) {
      i <- x$getinverse() ## attempts to get input object's inverse
      if(!is.null(i)) { ## checks if result is 'NULL'
            message("getting cached data")
            return(i) ## if not 'NULL' returns inverse from cache
      }
      data <- x$get() ## otherwise, solves for input object's inverse
      i <- solve(data, ...) 
      x$setinverse(i) ## sets inverse in input object
      i ## returns value of inverse to the parent environment
}


## Example for testing
m <- matrix(runif(10000), 100, 100) ## create matrix
m1 <- makeCacheMatrix(m) ## create 'correct format' matrix

cacheSolve(m1) ## calculate inverse for first time

cacheSolve(m1) ## retrieve inverse from cache

## Since the above matrix is not that large it is hard to 
## observe the time difference in solving for / retrieving
## the matrix's inverse.
