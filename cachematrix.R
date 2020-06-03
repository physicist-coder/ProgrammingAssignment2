## is calculated and printed out, if inverse is already present it is printed out
## this is a function which is used to get (or) set the cache matrix and inverse of the matrix
makeCacheMatrix <- function(a = matrix()){
    #initialzing
    inbuilt<-NULL;
    set_matrix<-function(y){
        a<<-y;
        inbuilt<<-NULL;
    }
    #getting the matrix
    get_matrix=function() a
    #setting the matrix inverse to the cache
    set_inv=function(inv) inbuilt<<-inv
    #getting the cache
    get_inv=function() inbuilt
    list(set_matrix=set_matrix,get_matrix=get_matrix,set_inv=set_inv,get_inv=get_inv)
}

#this function checks if the given matrix is already present or not
#if present it fetches data from the previous matrix
#if not present it calculates the inverse and adds that to the cache

cacheSolve<-function(x,...){
    #checks if the matrix inverse is already present, if yes print it out
    inverse_matrix=x$get_inv()
    if(!is.null(inverse_matrix)){
        return(inverse_matrix)
    }
    #if not solves for the matrix, prints it and stores it in the cache
    matrix<-x$get_matrix()
    inverse_matrix=solve(matrix)
    x$set_inv(inverse_matrix)
    inverse_matrix
}