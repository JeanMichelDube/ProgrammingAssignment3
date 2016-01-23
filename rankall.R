rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  state = as.character(unique(data$State))
  
  ## Check that state and outcome are valid
  if (outcome == "heart attack"){o=11} else if (outcome == "heart failure") {o=17} else if (outcome == "pneumonia") {o=23} else {stop("invalid outcome")}
  
  findRank <- function(state, o, num = "best") {
    #if number est plus grand que length, retourner NA
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    ## Check that state and outcome are valid
    selectState <- data[data$State == state, ]
    
    ## Return hospital name in that state with the given rank
    cutNa=subset(selectState,suppressWarnings(is.na(as.numeric(selectState[,o]))==FALSE))
    secondTable = cutNa[order (cutNa[,2]),]
    finalTable = secondTable[order (as.numeric(secondTable[,o])),]
      
    ## 30-day death rate
    if (num=="best") {out=finalTable[1,2]} else if (num=="worst") {out = tail(finalTable,n=1L)[1,2]} else {out = finalTable[num,2]}
    
    out
  }
  
  ## For each state, find the hospital of the given rank
  outputfonction = lapply(as.character(state),findRank,o=o,num=num)
  
  ## Return a data frame with the hospital names and the
  result=do.call(rbind,outputfonction)
  
  ## (abbreviated) state name
  final=cbind(result,state)
  superfinal=final[order(final[,2]),]
  superfinal
}