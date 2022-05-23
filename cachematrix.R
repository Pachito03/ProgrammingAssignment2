# patricio carrillo  MAy 23, 2022
# file contains two functions makeCacheMatrix and cacheSolve
#
# makeCacheMatrix 
#      sets up variables to cache a matrix and its inverse
#      returns a list of functions to 
#                set and get the stored matrix 
#                and to set and get the stored inverse
#      the list is the argument for the cacheSolve function
#
# cacheSolve
#      solves the inverse of a matrix if:
#         1. not done before
#         2. matrix has not changed since last solved
#      else 
#         gets the cached inverse 


################################################################# 
#                                                               #
#                        makeCacheMatrix                        #
#                                                               #
# function sets up "cache variables" for matrix and its inverse #
# defines list of functions to be used by cacheSolve function   #
#                                                               #
#################################################################

makeCacheMatrix <- function (x = matrix())
        
#        
#arguments 
#       square matrix to be inversed
#
#return / cache
#
#      Cache_A : cache for original matrix
#      Cache_inv: cache for inverse of matrix 
#      return (list of functions)

{
        # creates and initializes "global" matrices
        
        Cache_A <<-  NULL
        Cache_inv <<- NULL
        
        #  functions to store and retrieve matrix
        
        set <- function(y) 
        {
                Cache_A <<- y
        }
        
        get <- function() x
        
        #  functions to store and retrieve inverse of matrix
        
        setinv <- function(mat) 
        {
                Cache_inv <<- mat
        }
        
        getinv <- function() Cache_inv
        
        # return list of functions
        
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

################################################################# 
#                                                               #
#                          cacheSolve                           #
#                                                               #
#      solves the inverse of a matrix if:                       #
#         1. not done before                                    #
#         2. matrix has not changed since last solved           #
#      else                                                     #
#         gets the cached inverse                               #
#                                                               #
#################################################################


cacheSolve <- function(x, ...)

#
# argument
#       list of functions a.k.a. "matrix" created with
#       function makeCacheMatrix
#
# return
#       inverse of matrix
        
{
        
        # set control boolean and get stored matrices
        
        get_cache <- TRUE
        A_inv <- x$getinv()
        A <- x$get()
        
        # check if A_inv is null
        
        if(is.null(A_inv)) 
        {
                get_cache <- FALSE        
        }
        else
        {
                # check if matrix is the same
                
                if(!identical(Cache_A, A))
                {
                        message("Matrix was changed")
                        get_cache <- FALSE
                }
        }
        
        if(get_cache)
        {
                # if inverse was already solved return cached invers
                
                message("Getting cache")
                A_inv <- x$getinv()
                return (A_inv)
        }
        
        # if inverse was not solved (or matrix changed):  solve, store & return
        
        message("Processing")
        A <- x$get()
        A_inv <- solve(A, ...)
        x$set(A)
        x$setinv(A_inv)
        A_inv
}
