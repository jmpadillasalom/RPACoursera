best <- function(state, outcome) {
  mydata <-
    read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  mydata[,11] <- suppressWarnings(as.numeric(mydata[,11]))
  mydata[,17] <- suppressWarnings(as.numeric(mydata[,17]))
  mydata[,23] <- suppressWarnings(as.numeric(mydata[,23]))
  
  
  states <- unique(mydata$State)
  conditions <- c('heart attack', 'heart failure', 'pneumonia')
  
  if (!state %in% states) {
    stop('invalid state')
  }
  if (!outcome %in% conditions) {
    stop('invalid outcome')
  }
  
  #Subseting data for the correct state
  subsetbyState <- mydata[grep(state, mydata$State, ignore.case = T),]
  
  
  if (outcome == 'heart attack') {
    mycol <- 11
  }
  if (outcome == 'heart failure') {
    mycol <- 17
  }
  if (outcome == 'pneumonia') {
    mycol <- 23
  }
  
  retval <- subsetbyState[order(subsetbyState[,mycol],subsetbyState[,2]),]
  retval <- na.omit(retval)
  retval[1,2]
}