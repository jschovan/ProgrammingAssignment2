###
### This is cachematrix.R, for coursera's rprog-007, Project 2.
###
### Example usage: 
###       # 1. Create a matrix, x:
###       > x <- matrix(rnorm(9), nrow=3, ncol=3);
###       # 2. Create special "matrix" object, sx:
###       > sx <- makeCacheMatrix(x);
###       # 3. Get the special "matrix":
###       > sx$get();
###       # 4. Get its inverse matrix to sx, calculate it:
###       > cacheSolve(sx);
###       # 5. Get the inverse matrix to sx, use the cached value:
###       > cacheSolve(sx);
###       # 6. Calculate product of x and its inverse, it should be ~ identity matrix: 
###       > x %*% sx$get_inverse();
### 


### makeCacheMatrix: This function creates a special "matrix" object that 
###   can cache its inverse.
###
###   Input: a matrix, x.
###
###   Output: a list of functions to
###     * set the matrix
###     * get the matrix
###     * set the matrix inverse
###     * get the matrix inverse
###
makeCacheMatrix <- function(x = matrix()) {
  
  ### variable to store the inverse matrix
  inverse_matrix <- NULL;
  
  ### set the value of the matrix
  set <- function(y) {
    x <<- y;
    inverse_matrix <<- NULL;
  }
  
  ### get the value of the matrix
  get <- function() x;
  
  ### set the value of the inverse matrix
  set_inverse <- function(invmatrix) inverse_matrix <<- invmatrix;
  
  ### get the value of the inverse matrix
  get_inverse <- function() inverse_matrix;
  
  ### list of matrix functions
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse);
}


### cacheSolve: This function computes the inverse of the special "matrix" 
###   returned by makeCacheMatrix above. If the inverse has already been 
###   calculated (and the matrix has not changed), then the cachesolve should 
###   retrieve the inverse from the cache.
###
###   Input: a matrix, x.
###
###   Output: calculate the inverse matrix of matrix x. If the inverse matrix 
###     has already been calculated (and cached), use the version from cache.
###
cacheSolve <- function(x, ...) {
  
  ### try to get inverse_matrix of x from cache
  inverse_matrix <- x$get_inverse();
  
  ### the inverse_matrix of x is available in the cache, return cached version
  if(!is.null(inverse_matrix)) {
    
    message("Getting inverse matrix from the cache!");

    ## Return a matrix that is the inverse of 'x'
    return(inverse_matrix);

  } else {
    ### the inverse_matrix of x was not stored in the cache, calculate it
    message("Calculating the inverse matrix, cached one not found!");
    
    #### calculate inverse matrix from x
    inverse_matrix <- solve(x$get());
    
    #### set inverse matrix of x
    x$set_inverse(inverse_matrix);
    
    ## Return a matrix that is the inverse of 'x'
    return(inverse_matrix);

  }
}


### 1. Create a matrix, x:
x <- matrix(rnorm(9), nrow=3, ncol=3);
### 2. Create special "matrix" object, sx:
sx <- makeCacheMatrix(x);
### 3. Get the special "matrix":
sx$get();
### 4. Get its inverse matrix to sx, calculate it:
cacheSolve(sx);
### 5. Get the inverse matrix to sx, use the cached value:
cacheSolve(sx);
### 6. Calculate product of x and its inverse, it should be ~ identity matrix:
x %*% sx$get_inverse();


