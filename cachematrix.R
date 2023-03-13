## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Strange because the below function is never used explicitly. 
  set <- function(y){
    x <<- y 
    i <<- NULL
  }
  # Returns matrix that we gave as input. Needed because we have to pass the value of our input matrix to
  # another function
  get <- function() x
  
  setinverse <- function(inverse) 
    i <<- inverse
  
  getinverse <- function() 
    i
  # Returns a list of named functions that we can invoke when needed.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # Check to see if inverse has been pre-computed
  if(!is.null(i)){
    message("Getting cached data...")
    return(i)
  }
  # If not, compute inverse and store in our special matrix object using setinverse()
  data <- x$get()
  i <- solve(data, ...) # solve() finds inverse of a matrix
  x$setinverse(i)
  i
}

# To test, run 
# cached_matrix <- makeCacheMatrix(my_matrix)
# cacheSolve(cached_matrix)
# Running cacheSolve the first time for a large matrix will take some time.
# If you run it again, it will be instant. 
        ## Return a matrix that is the inverse of 'x'
}
