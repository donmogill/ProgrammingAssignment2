# makeCacheMatrix and cacheSolve
# take a matrix and cache it's inverse

# makeCacheMatrix takes in a matrix
# it stores the matrix and inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  
  
  x_inv <- NULL
  
  # set the matrix in cache
  set <- function(y) {
    x <<- y
    
    # clear out the old cached inverse
    x_inv <<- NULL
  }
  
  # get the matrix from cache
  get <- function() x
  
  # set the inverse matrix in cache
  setInverseMatrix <- function(inverseMatrix) x_inv <<- inverseMatrix
  
  #get the inverse matrix from cache
  getInverseMatrix <- function() x_inv
  
  # expose the four functions
  list(set = set, 
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}



## Return a matrix that is the inverse of 'x'


# cacheSolve takes in a 'makeCacheMatrix' object
# it tries to get the cache inverse first

cacheSolve <- function(x, ...) {
  
  #checkout x_inv - if not null then return it
  x_inv <- x$getInverseMatrix()
  
  if(!is.null(x_inv)) {
    message("getting cached MATRIX data")
    return(x_inv)
  }
  
  message("running solve")
  
  # we have to get the inverse using the solve function
  data <- x$get()
  x_inv <- solve(data)
  
  # store it in cache
  x$setInverseMatrix(x_inv)
  x_inv
  
}

m = rbind(c(4, 7), c(2, 6))
cmc = makeCacheMatrix(m)

cs = cacheSolve(cmc)
cs
cs


