## This pair of functions has to enabled caching the inversion of matrix,
## so inversion of matrix will be comupted only once for matrix
## if the matrix doesn't change.


## This code uses examples from the course as templates
## to write assigned functions.




## This function creates a special "matrix" object that can cache its inverse.
## Description from the course.


## makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to

## set the value of the matrix - set
## get the value of the matrix - get

## set the value of the inverse of a matrix - setinv
## To get value of the inverse of a matrix function cacheSolve should be used:
## getinv can return NULL when matrix inverse hasn't been computed yet.

## get the value of the inverse of a matrix - getinv
## Use of setinv can lead to errors in cacheSolve so it should be done
## with great caution, to ensure that given value is the correct matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      ## For new created matrix inverse hasn't been computed yet.
      inv <- NULL
      
      ## set function can change the value of matrix x to y.
      set <- function(y) {
            x <<- y
            ## If matrix changes then we don't know the new inverse.
            ## We don't compute the inverse here, because the matrix can
            ## change multiple times before the value of inverse will be needed.
            inv <<- NULL
      }
      
      get <- function() x
      
      ## setinv can simply change value of inv to inversion.
      setinv <- function(inversion) inv <<- inversion
      
      getinv <- function() inv
      
      ## Function returns list that gives access to the matrix and it's inverse
      ## throught the four declared functions.
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## Description from the course.

cacheSolve <- function(x, ...) {
      
      
      inv <- x$getinv()
      
      ## First it is checked whether matrix inverse (inv) is already computed.
      ## NULL means that the matrix inverse has to be computed.
      if(!is.null(inv)) {
            ## If inv isn't NULL then the matrix inverse was already computed
            ## and it's value can be returned.
            return(inv)
      }
      
      ## To compute inverse the matrix value is needed.
      data <- x$get()
      
      ## We compute inverse.
      inv <- solve(data, ...)
      
      ## Store it's value for future.
      x$setinv(inv)
      
      ## And return computed inverse.
      inv
}

## The course URL:
## https://class.coursera.org/rprog-033/human_grading/view/courses/975107/assessments/3/submissions
