## These two functions are aimed to set a matrix (assumed 
## to be invertible) and compute and cache the inverse of
## the matrix, so that the inverse of the matrix does not
## have to be computed again for later needs.

## The first function creates a special "vector", which is
## really a list of containing a function to
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
            x<<-y
            m<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) m<<-inverse
        getInverse<-function() m
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## When the matrix has been created in "makeCacheMatrix" 
## function, the inverse of the matrix could be computed 
## in "cacheSolve" function. If the computation has been
## performed before, the function gets the cached result. 
## Otherwise, it will read matrix, compute the inverse of
## the matrix, cache and return the result.

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

# To test the function
t<-makeCacheMatrix()
class(t)
summary(t)
t$set(matrix(sample(1:50,9),nrow=3,ncol=3))
t$get()
t$getInverse()
cacheSolve(t)
cacheSolve(t)