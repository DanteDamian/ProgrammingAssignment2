## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                m<-NULL
                set<-function(y){
                  x<<-y
                  m<<-NULL
                }
                get <- function() x
                setsolve <- function(inverse) m<<- inverse
                getsolve<- function() m
                list(set = set,
                     get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## Calculate the inverse of a matrix in cache , and if exits in a second requirement, use this cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
            m<- x$getsolve()
            if (!is.null(m)) {
                    message("getting cached data")
                    return(m)
}
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
}

