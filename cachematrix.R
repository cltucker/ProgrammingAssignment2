## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##this function creates a list containing the function to 
## set the values of the matrix x, get those values, 
## set the inverse of that matrix, and get that new matrix.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
##If there is a cached inverse matrix, that matrix is returned
##and the fuction is exited.  Otherwise, the inverse 
##matrix is calculated and then returned.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)){
            message("getting cached inverse matric")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}

##Below I added a few lines to test the program behavior:
x <- matrix(rnorm(9),3,3)
xstar<-makeCacheMatrix(x)
y<-cacheSolve(xstar)
##now we want to test that y is the inverse of x.  If we multiply the 
##matrices, the Identity matrix should be returned.
z<-round(x %*% y, 1))
z
##now we want to test that the matrix is cached, and the correct matrix 
## is returned.
y2<-cacheSolve(xstar)
if(sum(y2-y) == 0) {"Success"} else {"Try again"}


                     