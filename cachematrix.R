## Second program assignment
## Usage of lexicaal scoping to cache results

## Function gets the matrix, calculates its 
## inversion (OOTB function "solve") and caches 
## result as variable "inv" in function(or global?)
## envirnoment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## function loads the variable "inv" from cache
## fucntion checks whether exists
## if yes - just reuses it
## else - calculates it using OOTB "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached inversion")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}


