## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## This function creates a list that stores fuctions that sets and gets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {  ## x=matrix() is used to avoid an error in get()
        m<- NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)
             
}


## This function calculates the inverse of the matrix. Can take it from the cache or calculating it (if is NULL)

cacheSolve <- function(x, ...) {
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m
}
