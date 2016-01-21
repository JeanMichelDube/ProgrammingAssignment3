best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  if (is.element(as.character(state),data$State)>0) {selectState <- data[data$State == state, ]} else {stop("invalid state")}
  if (outcome == "heart attack"){o=11} else if (outcome == "heart failure") {o=17} else if (outcome == "pneumonia") {o=23} else {stop("invalid outcome")}
  
  ## Return hospital name in that state with lowest 30-day death rate
  m=suppressWarnings(na.omit(as.numeric(selectState[,o])))
  s <- suppressWarnings(subset(selectState,as.numeric(selectState[,o])==min(m),Hospital.Name))
  sort(s$Hospital.Name)
  as.character(s[1,1])
}

