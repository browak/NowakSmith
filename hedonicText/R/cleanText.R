#' A basic function to process text
#'
#' This function allows you to count flex grams given a string vector
#' @param TEXT a string vector
#' @param lower convert all characters in TEXT to lower-case?  Defaults to T
#' @param removeNumbers remove all numbers from TEXT?  Defaults to T
#' @param removePunctuation remove all PUNCTUATION from TEXT?  Defaults to T
#' @param removeStopWords remove all stop words from TEXT?  Defaults to T.  Default list of stop words is the "en" list in the tm package. 
#' @param removeSingleLetters remove all single letters from TEXT?  Defaults to T.
#' @param verbose a logical vector for output to console.  Defaults to F.
#' @keywords text processing
#' @export
#' @examples
#' cleanText(remarks)
#' 
#' 
cleanText <- function(TEXT , 
                      lower=NULL , 
                      removeNumbers=NULL , 
                      removePunctuation=NULL , 
                      removeStopWords=NULL,
                      removeSingleLetters=NULL,
                      verbose=T)
{
require('tm')
### is TEXT a character string?
if(class(TEXT)!="character") stop("TEXT is not a character class")
### setup
if(missing(lower)) lower <- T
if(missing(removeNumbers)) removeNumbers <- T
if(missing(removePunctuation)) removePunctuation <- T
if(missing(removeStopWords)) removeStopWords <- T
if(missing(removeSingleLetters)) removeSingleLetters <- T
if(missing(verbose)) verbose <- F
### step 1
if(lower & verbose) print("convert to lower case")
if(lower) TEXT <- tolower(TEXT)
### numbers
if(removeNumbers & verbose) print("remove numbers")
if(removeNumbers) TEXT <- gsub("[0-9]"," ",TEXT)
### punctuation
if(removePunctuation & verbose) print("remove punctuation")
if(removePunctuation) TEXT <- gsub("[[:punct:][:blank:]]+"," ",TEXT)
### stop words
if(removeStopWords & verbose) print("remove stop words")
if(removeStopWords) TEXT <- removeWords(TEXT,stopwords(kind="en"))
### single letters
if(removeSingleLetters & verbose) print("remove single letters")
if(removeSingleLetters) TEXT <- removeWords(TEXT,letters)
### remove white space
if(verbose) print("remove white space")
TEXT <- gsub("[[:blank:]]+"," ",TEXT)
TEXT <- trimws(TEXT)
### output
return(TEXT)
}