## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL               ##initialize inv as NULL
        set<-function(y){       ##define the set function 
                x<<-y
                inv<<- NULL
        }
        get<-function()x
        setinverse<-function(inverse) inv<<- inverse ##Define the get function to output inverse of martix
        getinverse<-function()inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## require to use $ operator in next function

}


## This function computes the inverse of special "matrix" returned by makeCacheMatrix.
## If inverse has already been calculated and matrix remains the same, cache solve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<- solve(data, ...) ## to compute the inverse of matrix
        x$setinverse(inv)
        inv ## final output
}

