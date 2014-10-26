## This function caches potentially time consuming function in makeCacheMatrix function and retrieves
## the inverse of the matrix from cache by cacheSolve function if available.
## Otherwise it computes inverse of special matrix in cacheSolve function
## Scoping rule is used to manipulate and preserve the state inside of a R object
## Function creates a special matrix object that can cache its inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
m <- NULL ## Creates variable in local environment
set <- function(y) {
x <<- y ## Supperassigns value of y to x, where x is makeVector()'s input
m <<- NULL ## If set() is called, a new vector will be stored in x, replacing the
## existing value
}
get <- function() x ## This can be called to retrieve the values of x from makeVector.
## cachemean will call on this function to provide it with values
## needed for computation
setmean <- function(mean) m <<- mean ## This sets m to mean in the parent env (makeVector())
getmean <- function() m ## R will look for the value of m in getmean()
## R won't find one within getmean, so it'll look
## to the parent environment makeVector for m value
list(set = set, get = get, ## This is needed to make functions public
setmean = setmean, ## Essentially, this allows these functions to be called
getmean = getmean) ## outside the local environment
}
## Function computes the inverse of special matrix returned by "makeCacheMatrix" function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
m <- x$getmean() ## This calls the getmean() function from x. x is a container environment that
## consists of a list of functions and variables defined in the makeVector() environment.
if(!is.null(m)) { ## This checks if m has an existing value. If TRUE, then return that value.
message("getting cached data")
return(m) ## If there's no existing value, this function will calculate it below
}
data <- x$get() ## Calls get() function from x, which won't have an m value
m <- mean(data, ...) ## Computes mean from the retrieved values via x$get()
x$setmean(m) ## With the new mean, setmean() will be called to to update m in makeVector()
## Recall: setmean() supperassigns m in makeVector(), thus “caching” the value
m ## Prints the new mean
}
