rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  if (is.element(as.character(state),data$State)>0) {selectState <- data[data$State == state, ]} else {stop("invalid state")}
  if (outcome == "heart attack"){o=11} else if (outcome == "heart failure") {o=17} else if (outcome == "pneumonia") {o=23} else {stop("invalid outcome")}
  
  ## Return hospital name in that state with the given rank
  cutNa=subset(selectState,suppressWarnings(is.na(as.numeric(selectState[,o]))==FALSE))
  secondTable = cutNa[order (cutNa[,2]),]
  finalTable = secondTable[order (as.numeric(secondTable[,o])),]
  
  ## 30-day death rate
  if (num=="best") {out=finalTable[1,2]} else if (num=="worst") {out = tail(finalTable,n=1L)[1,2]} else {out = finalTable[num,2]}
  out
}