## This function creates a special "matrix" which can cache a matrix inverse.

makeCacheMatrix <- function(my_matrix = matrix()) {
        inverse_matrix <- NULL
        set_matrix <- function(x) {
                my_matrix <<- x;
                inverse_matrix <<- NULL;
        }
        get_matrix <- function() return(my_matrix);
        set_inverse <- function(my_inv) inverse_matrix <<- my_inv;
        get_inverse <- function() return(inverse_matrix);
        return(list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse))
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix is unchanged), then 'cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(my_matrix, ...) {
        inverse_matrix <- my_matrix$get_inverse()
        if(!is.null(inverse_matrix)) {
                message("Getting cached data...")
                return(inverse_matrix)
        }
        my_data <- my_matrix$get()
        invserse_matrix <- solve(my_data, ...)
        my_matrix$set_inverse(inverse_matrix)
        return(inverse_matrix)
}