## Functions for cached storage/retrieval of matrix variables 
## and their inverted version. Refer to detailed description below



makeCacheMatrix <- function(x = matrix(,0,0),globen=FALSE) {
##********************************************************************
## Function that returns a list of functions, aka closures,  
## for cashing and retrieving matrixes 
## Design based on prototype examples for Coursera R-programming
## course, assigment2, Mar 2015 
##******************************************************************** 
## Arguments:
##          X should be an invertible square matrix. Default = dim[0,0]
##          globen controls whether variables shoot be placed in the
#           creation (default) or the global environment 
## Methods  :
##          $set(y) Save y as the cached matrix MM
##          $get()  Retrieve the cached matrix MM
##          $setInvMat(y) Save y as the cached matrix IMM (see usage ) 
##          $getInvMat() Retrieve the cached matrix IMM (see usage )
##********************************************************************
## Usage    :
##          The function can be assigned to a list object either
##          with and empty argument:    
##          ex0: MyListObj <- makeCacheMatrix() 
##          or alternatively with with a matrix to be saved to the
##          cached variable MM
##          ex1: MyListObj <- makeCacheMatrix(myMat)
##          Both these assigments will reset the cached variable IMM
##          used to hold the inverted matrix to NULL flagging the
##          need for matrix inversion of the new matrix
##    
##          Once assigned, the variable MM can be set(reset) or
##          retrieved using the setMat() and the getMat() functions
##          The setMat() function will also initiate variable IMM
##          to NULL, which is used by the companion cacheSolve()
##          to flag that a new matrix has been saved and should    
##          be inverted 
##          ex3: MyListObj$setMat(myMat1) 
##          Setmat() must have an aguametn     
##          ex4: myMat2 <- MyListObj$getMat()
##
##          The $setInvMat() and $getInvMat() will set / retrieve
##          the cached variable IMM. Although these function can be used
##          in much the the same way as the set() and get() functions 
##          they are intended as an interface for the cacheSolve() 
##          function to facilitate caching and high speed retrieval of 
##          inverse matrixes          
##    
##********************************************************************
## Version  : 
##          0.001   2015-03-16/hpj
##                  The argument for makeCacheMatrix() and its set() 
##                  function should be restricted to invertible
##                  square matrices if to be used with the cacheSolve()
##                  matrix inversion function since no checks are 
##                  performed to confirm this assumption according  
##                  to design specs
##          0.002   2015-03-22/hpj
##                  Modified method of checking new vs cached version
##                  to use the identical() function insteas of matrix
##                  element sums. Although slightly slower, the latter
##                  method is more accurate    
##********************************************************************        
 
    #Initiate the matrix M and its checksum and the IMM matrix variables
    #The value of globen determines if the variables are in the 
    #creation environment (globen == FALSE) which is default or in the
    #global envirionment (globen == TRUE) 
    #NOte that creation in the global environement may cause conflicts 
    #as multiple instances of the list objects created using
    #makeCacheMatrix() will refer i.e share the same variables
    
    if (globen == FALSE) {
        IMM <- NULL
        MM  <- NULL
    }
    
    #Set() is a function used to cache the variable(matrix) MM
    #To aviod repeatedly writing the same matrix
    #and thus triggering a recompute of the inverse matrix
    #the new matrix is compared to the cashed version if
    #any using the indentical() function 
    #For all calls of set(), except the first, if new matrix 
    #is not identical the cache is updated with the new version
    set <- function(y) {
        
        if (sum(dim(y)) == 0) { #
            #If Set() is called with an empty argument
            #reset allcashed variables
            IMM <<- NULL
            MM <<- NULL
       } else {
            #Compare new and cached versions of the matrix
            #and update if different 
            if (!identical(y,MM)) {
                #Cache the new matrix in variable MM 
                MM <<- y
                #Assign NULL to the cached IMM variable
                #to flag need for new inversion
                IMM <<- NULL
            }
        }    
    }
    
    #Function to retrieve cached variable MM
    get <- function() MM
    
    #Function to save inverted matrix to cached variable IMM
    #intended for cacheSolve() 
    setInvMat <- function(M2cahce) IMM <<- M2cahce

    #Function to retieve cached variable IMM 
    #(intended for cacheSolve() )
    getInvMat <- function() IMM
    
    
    
    #Call set() function to to create MM matrix 
    set(x)
    
    
    #List of functions returned during creation
    list(setMat = set, getMat = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)  


}




cacheSolve <- function(MCMo, ...) {
##********************************************************************
## Function returns a (cached) matrix that is the inverse of 'x'
## Design based on prototype examples for Coursera R-programming
## course, assigment2, Mar 2015 
##******************************************************************** 
## Arguments:
##          MCMo is an list object with function closures created from
##          the an makeCacheMatrix() function 
## Values   :
##          mM the inverse of the matrix originally fetched using the
##          MCMo$getMat() function
##********************************************************************
## Version  : 
##          0.001   2015-03-16/hpj
##                  The matrix mM is assumed to be square and invetible
##                  According to design specs no checks are performed 
##                  to confirm this assumption
##********************************************************************                  
    #Request the cached inv.matrix 
    mM <- MCMo$getInvMat()
    
    if(!is.null(mM)) {
        #if M was not returned as NULL i.e. empty
        message("fetching cached matrix")
    } else {
        #if mM was returned as NULL i.e. not cached
        message("inverting and cashing matrix")
        #Fecth the matrix to be inverted
        mat.data <- MCMo$getMat()
        #Attempt to invert the matrix
        mM <- solve(mat.data, ...)
        #Cache the inverted matrix
        MCMo$setInvMat(mM)
    }
    #Return the inverted matrix
    return(mM)
}




