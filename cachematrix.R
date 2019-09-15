## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## This function creates a list that stores fuctions that sets and gets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {  ## x=matrix() is used to avoid an error in get()
        Inv<- NULL
        set<-function(y){
                x<<-y
                Inv<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) Inv<<-solve
        getsolve<-function() Inv
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)
             
}


## This function calculates the inverse of the matrix. Can take it from the cache or calculating it (if is NULL)

cacheSolve <- function(x, ...) {
        Inv<-x$getsolve()
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        data<-x$get()
        Inv<-solve(data,...)
        x$setsolve(Inv)
        Inv
}