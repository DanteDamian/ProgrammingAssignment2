## This Function allow to calculate de inverse matrix  and if the inverse is already calculated, the uses the cache
## 

## This function create a cache over the result of a inverse matrix

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

