## The below function creates a special "matrix" object that can cache 
# its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # set the value of the matrix
    x <<- y            
    m <<- NULL
  }
  # <<- is an operator which can be used to assign a value to an object 
  # in an environment that is different from the current environment
  
  get <- function() { # get the value of the matrix
    x
  }  
  
  # set the value of the mean
  setmatrix <- function(solve) {
    m <<- solve
  }
  
  # get the value of the mean
  getmatrix <- function() {
    m
  }
  
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## The below function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above.  If the inverse has already been 
#  calculated (and the matrix has not changed), then the cachesolve 
#  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  else {
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    return(m)
  }
}

## Testing:
# x <- makeCacheMatrix( matrix(1:4, nrow = 2, ncol = 2) )
# summary(x)
# x$get()
# cacheSolve(x)


# Source of the code below:
# http://gmlang.com/how-to-cache-a-matrix-inversion-in-r/

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

# Arguments used for test:

# set.seed(1110201)
# r = rnorm(1000000)
# mat1 = matrix(r, nrow=1000, ncol=1000)
# test(mat1)
