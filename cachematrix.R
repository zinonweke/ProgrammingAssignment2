
## THIS FUNCTION CHECKS IF THE INVERSE MATRIX HAVE BEEN STORED IN CACHE, ELSE 
## IT COMPUTES THE SPECIAL INVERSE MATRIX AND STORE IT IN CACHE.


## This first function returns a list of functions that set matrix, get matrix
## store calculated inverse and return from cache

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    set_mat <- function(new_mat){
        x <<- new_mat
        solved <<- NULL
    }
    get_mat <- function() x
    set_inverse <- function(inverse) solved <<- inverse
    get_inverse <- function() solved
    list(set_mat = set_mat, get_mat = get_mat,
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## This function check if the inverse have been calculated already, then returns the
## the inverse. Else, it computes the inverse matrix, returns and stores it for easy access

cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get_mat()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}
