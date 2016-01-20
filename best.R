best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  
  if (outcome == "heart attack"){o=11} else if (outcome == "heart failure") {o=17} else if (outcome == "pneumonia") {o=23} else {stop(best)}
  
  ## Return hospital name in that state with lowest 30-day death rate
  selectState <- data[data$State == state, ]
  m <- min(as.vector(selectState[,o]), na.rm = TRUE)
  s <- subset(selectState,as.vector(selectState[,o])==m,Hospital.Name)
  sort(s$Hospital.Name)
  as.character(s[1,1])
}
