#' Calucluate the most frequent flex-grams
#'
#' This function allows you to count flex grams given a string vector
#' @param TEXT a string vector
#' @param maxN the maximum number of unigrams allowed in a flexgram.  Defaults to 5.
#' @param minN the minimum number of unigrams allowed in a flexgram.  Defaults to 2.
#' @param minCount the minimum number of counts required for each flex-gram.  Defaults to min(floor(0.01*length(TEXT)) , 2).
#' @param verbose a logical vector for output to console.  Defaults to F.
#' @keywords flex-grams
#' @export
#' @examples
#' flexGramCount(remarks)
#' 
#' 
#' 
flexGramCount <- function(TEXT , 
                          maxN=NULL , 
                          minN=NULL , 
                          minCount=NULL , 
                          verbose=NULL)
{
require(ngram)
require(doParallel)
require(stringi)
### setup
if(missing(maxN)) maxN <- 5
if(missing(minN)) minN <- 2
if(missing(minCount)) 
{
minCount <- floor(0.01*length(TEXT))
if(minCount<2) minCount <- 2
manualMinCount <- F
} else {
manualMinCount <- T
}
if(missing(verbose)) verbose <- F 
###########################
### program starts here ###
gramList <- data.frame()
NVEC <- maxN:minN
for(N in NVEC)
{
count <- sapply(regmatches(TEXT, gregexpr(" ", TEXT)), length)
ng <- ngram(TEXT[N<=count] , n=N)
tab <- get.phrasetable(ng)
### check for too large minCount
if(max(tab$freq)<minCount & manualMinCount) 
{
message <- paste0("minCount is too large for ",N,"-grams. Consider lowering minCount. Skipping to ",(N-1),"-grams")
print(message)
next
}
if(max(tab$freq)<minCount & !manualMinCount)
{
message <- paste0("no ",N,"-grams found with frequency larger than minCount. Skipping to ",(N-1),"-grams")
if(N==2) message <- "no 2-grams found with frequency larger than minCount"
print(message)
next
}
###
tab <- subset(tab , minCount<=freq)
tab <- subset(tab , !grepl("-",ngrams))
tab$ngrams <- trimws(tab$ngrams)
tab$ngrams2 <- gsub(" ","-",tab$ngrams)
tab$N <- N
gramList <- rbind(gramList , tab)
### put in markers for n-grams
WORDS2 <- tab$ngrams
REPLACEMENT2 <- tab$ngrams2
iREPORT <- floor(0.1*length(TEXT))
TEXT2 <- foreach(i = 1:length(TEXT) , .combine=c) %do%
{
if(i%%iREPORT==0 & verbose)
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
