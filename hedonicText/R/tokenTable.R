#' A basic function to process text
#'
#' This function allows you to count flex grams given a string vector
#' @param B a coefficient vector from a glmnet fit.
#' @param ROUND round the coefficeints?  Defaults to T
#' @keywords make a table with tokens and loadings
#' @export
#' @examples
#' b <- coef(unigramFit , s='lambda.min')
#' tokenDataFrame <- tokenTable(b)
#' 
#' 
tokenTable <- function(B , ROUND=NULL)
{
if(missing(ROUND)) ROUND <- T
df <- data.frame(token=rownames(B),coefficient=as.numeric(B))
if(ROUND) df$coefficient <- round(df$coefficient,3)
df <- df[df$token!="",]
df <- df[df$token!="(Intercept)",]
df <- df[df$coefficient!=0,]
df <- df[order(-abs(df$coefficient)),]
return(df)
}
