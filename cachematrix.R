## [Put comments here that describe what your functions do]
##PROGRAMMING ASSIGNMENT 2

###MakeCacheMatrix takes the inverse of a matrix _
#matrix is cached for retrieval as retrieve

###CacheSolve checks in matrix is cached
#If matrix is cached, inverse is returned from memory
#else inverse is solved & returned

makeCacheMatrix <- function(x = matrix()) {
  finder<-NULL ####does not yet exist
  ###could use ginv() if available
    ##Function 1 of 4 --> set --> look up the value in the parent env
      set <- function(y) {
        x <<- y
        finder <<- NULL}       
    ##Function 2 of 4 --> get --> retrieve the value from env
      get <- function() x
   ##Function 3 of 4 --> setinv (record inv matrix later retrieval _
        ###using solve and set value)
      setinv <- function(solve) finder <<- solve
    ###Function 4 of 4 --> getinv --> retrieval
      getinv <- function() finder
    ###the functions to be called are stored as a list 
      list(set = set, get = get, setinv = setinv,
              getinv = getinv)}


  


cacheSolve <- function(x, ...) {
  ## Look up matrix to see if inversed cached
  ## Calculate if not cached
  ##Return a matrix that is the inverse of 'x'
  finder <- x$getinv()
  
  if(!is.null(finder)) {
    message("getting cached data")
    return(finder)
  } ###else
  data <- x$get()
  finder <- solve(data, ...)
  x$setinv(finder)
  finder
}
