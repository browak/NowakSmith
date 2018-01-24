#' Estimate a hedonic model
#'
#' This function allows you to estimate a hedonic model using an attribute matrix and a token matrix.
#' @param Y the dependent variable.
#' @param X a matrix or vector of attributes for the house.  This is not required.
#' @param M a matrix for the tokens.  This is required.
#' @keywords hedonic model, LASSO
#' @export
#' @examples
#' textFit <- hedonicWithText(logprice , attributeMat , tokenMat)
#'
#'
hedonicWithText <- function(Y , X=NULL , M=NULL)
{
require(Matrix)
require(glmnet)
### setup
if(missing(M)) stop("must supply a matrix for tokens")
if(missing(X)) W <- M
if(!missing(X)) 
{
  if(is.null(ncol(X))) nX <- 1
  if(!is.null(ncol(X))) nX <- ncol(X)
  W <- cBind(X,M)
}
nW <- ncol(W)
PENVEC <- rep(1,nW)
#PENVEC[1:nX] <- 0
### estimate
cv <- cv.glmnet(W , Y , penalty.factor=PENVEC) 
return(cv)
}