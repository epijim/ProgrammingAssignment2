## makeCacheMatrix 
#   This will save the matrix, return the saved matric, cache the inverse for laters,
#   and spit out the results in a list

makeCacheMatrix <- function(x = matrix()) {
  ## Make blank object
  matrix <- NULL
  ## Put a matrix in
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  ## Pull out the matrix we put in
  get <- function() x
  ## Keep the solved
  setinversed <- function(solve) matrix <<- solve
  ## Put the results into a list to get out of function
  getinversed <- function() matrix
  ## And it's done
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)
}


## cacheSolve
# Check if cached to save time  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' if it's available
  matrix <- x$getinversed()
  ## Is the data there?
  if(!is.null(matrix)) {
    message("Data is inbound")
    return(matrix)
  }
  ## If it isn't - get the matrix
  data <- x$get()
  ## Solve it
  matrix <- solve(data, ...)
  ## Save it
  x$setinversed(matrix)
  ## Show it
  matrix
  
}

#### CODE BELOW IS ONLY FOR TESTING

# A <- matrix(1:4,2,2)      ## make a square matrix A
# Am <- makeCacheMatrix(A)  ## set matrix A into cache
# Ai <- cacheSolve(Am)      ## solve matrix using cache
# Ai                        ## inverse matrix
# solve(A)                  ## compare result - should be the same!