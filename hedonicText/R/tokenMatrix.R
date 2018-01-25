#' A basic function to process text
#'
#' This function allows you to count flex grams given a string vector
#' @param TEXT a string vector.
#' @param TOKENLIST a string vector with the tokens to look for in TEXT.
#' @param verbose a logical vector for output to console.  Defaults to F.
#' @keywords make a matrix of indicator variables for each token
#' @export
#' @examples
#' M <- tokenMatrix(remarks , realEstateTokens)
#' 
#' 
tokenMatrix <- function(TEXT , TOKENLIST , verbose=NULL)
{
require(foreach)
require(Matrix)
###
if(missing(verbose)) verbose <- F
###
tokenREPORT <- floor(0.05*length(TOKENLIST))
matrixList <- foreach(i = 1:length(TOKENLIST)) %do% 
{
TOKEN <- TOKENLIST[i]
if(i%%tokenREPORT==0 & verbose) print(paste0("on ",i," of ",length(TOKENLIST)))
it  <- grep(TOKEN,TEXT)
it <- unique(it)
it
}
### keepers
KEEPERS <- which(sapply(matrixList,length)!=0)
TOKENLIST <- TOKENLIST[KEEPERS]
matrixList <- matrixList[KEEPERS]
### make matrix
bigI <- unlist(matrixList)
nj <- sapply(matrixList , length)
bigJ <- rep(1:length(matrixList) , nj)
M <- sparseMatrix(i=bigI , j= bigJ , dims=c(length(TEXT),length(matrixList)))
colnames(M) <- TOKENLIST
return(M)
}
