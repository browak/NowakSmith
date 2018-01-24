#' Calucluate the most frequent flex-grams
#'
#' This function allows you to count flex grams given a string vector
#' @param TEXT a string vector
#' @param MAXN the maximum number of unigrams allowed in a flexgram.  Defaults to 5.
#' @param MINN the minimum number of unigrams allowed in a flexgram.  Defaults to 2.
#' @param MINCOUNT the minimum number of counts required for each flex-gram.  Defaults to min(floor(0.01*length(TEXT)) , 2).
#' @param VERBOSE a logical vector for output to console.  Defaults to F.
#' @keywords flex-grams
#' @export
#' @examples
#' flexGramCount(remarks)
#' 
#' 
#' 
flexGramCount <- function(TEXT , MAXN=NULL , MINN=NULL , MINCOUNT=NULL , VERBOSE=NULL)
{
require(ngram)
require(doParallel)
require(stringi)
### setup
if(missing(MAXN)) MAXN <- 5
if(missing(MINN)) MINN <- 2
if(missing(MINCOUNT)) 
{
MINCOUNT <- floor(0.01*length(TEXT))
if(MINCOUNT<2) MINCOUNT <- 2
}
if(missing(VERBOSE)) VERBOSE <- F 
###########################
### program starts here ###
gramList <- data.frame()
NVEC <- MAXN:MINN
for(N in NVEC)
{
count <- sapply(regmatches(TEXT, gregexpr(" ", TEXT)), length)
ng <- ngram(TEXT[N<=count] , n=N)
tab <- get.phrasetable(ng)
tab <- subset(tab , MINCOUNT<=freq)
tab <- subset(tab , !grepl("-",ngrams))
tab$ngrams <- trimws(tab$ngrams)
tab$ngrams2 <- gsub(" ","-",tab$ngrams)
tab$N <- N
gramList <- rbind(gramList , tab)
### put in markers for n-grams
WORDS2 <- tab$ngrams
REPLACEMENT2 <- tab$ngrams2
iREPORT <- floor(0.01*length(TEXT))
TEXT2 <- foreach(i = 1:length(TEXT) , .combine=c) %do%
{
if(i%%iREPORT==0 & VERBOSE)
{
REPORT <- paste(N,"-grams ",i," of ",length(TEXT),sep="")
print(REPORT)
}
TEXTi <- stri_replace_all_fixed(TEXT[i],WORDS2,REPLACEMENT2,vectorize_all=F)		
TEXTi
}
### update text
TEXT <- TEXT2
}
### program ends here ###
##########################
### append new n-grams to gramList
gramList$ngrams2 <- NULL
gramList$prop <- NULL
gramList <- gramList[order(-gramList$freq),]
### return the gram counts
return(gramList)
}
