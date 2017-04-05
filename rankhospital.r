rankhospital <- function(state, outcome, num = "best") {
  
  mydata <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  mydata[,11] <- suppressWarnings(as.numeric(mydata[,11]))
  mydata[,17] <- suppressWarnings(as.numeric(mydata[,17]))
  mydata[,23] <- suppressWarnings(as.numeric(mydata[,23]))
  
  
  states <- unique(mydata$State)
  conditions <- c('heart attack', 'heart failure', 'pneumonia')
  
  if (!state %in% states) { stop('invalid state') }
  if (!outcome %in% conditions) { stop('invalid outcome') }
  
  if (outcome == 'heart attack' ) { mycol <- 11 }
  if (outcome == 'heart failure' ) { mycol <- 17 }
  if (outcome == 'pneumonia' ) { mycol <- 23 }
  
  subsetbyState <- mydata[grep(state, mydata$State, ignore.case=T),]
  ordered_data_for_state <- order(subsetbyState[mycol], subsetbyState$Hospital.Name, na.last=NA)
  
  if (num == "best") {
    as.character(subsetbyState$Hospital.Name[ordered_data_for_state[1]])
  } else if (num == "worst") {
    as.character(subsetbyState$Hospital.Name[ordered_data_for_state[length(ordered_data_for_state)]])
  } else if (is.numeric(num)) {
    as.character(subsetbyState$Hospital.Name[ordered_data_for_state[num]])
  } else {
    stop("invalid outcome")
  }
}