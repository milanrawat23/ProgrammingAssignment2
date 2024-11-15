# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse as NULL
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y  # Update the matrix with a new value
    inv <<- NULL  # Reset the cached inverse to NULL
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list containing all the functions defined above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the matrix (or retrieve from cache)
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse
  }
  
  # If not cached, compute the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse using the solve() function
  x$setInverse(inv)  # Cache the computed inverse
  
  return(inv)  # Return the computed inverse
}
