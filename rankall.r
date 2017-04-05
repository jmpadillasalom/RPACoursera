rankall <- function(outcome, num = "best") {
  mydata <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  mydata[,11] <- suppressWarnings(as.numeric(mydata[,11]))
  mydata[,17] <- suppressWarnings(as.numeric(mydata[,17]))
  mydata[,23] <- suppressWarnings(as.numeric(mydata[,23]))
  
  
  
  conditions <- c('heart attack', 'heart failure', 'pneumonia')
  
  if (!outcome %in% conditions) { stop('invalid outcome') }
  
  if (outcome == 'heart attack' ) { mycol <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" }
  if (outcome == 'heart failure' ) { mycol <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" }
  if (outcome == 'pneumonia' ) { mycol <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" }
  
  data_by_state <- split(mydata[, c("Hospital.Name", "State", mycol)], mydata$State)
  
  rank_hospital <- function(state_data, num) {
    ordered_state_data <- order(state_data[3], state_data$Hospital.Name, na.last=NA)
    
    if (num == "best") {
      state_data$Hospital.Name[ordered_state_data[1]]
    } else if (num == "worst") {
      state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
    } else if (is.numeric(num)) {
      state_data$Hospital.Name[ordered_state_data[num]]
    } else {
      stop("invalid num")
    }
  }
  
  pre_result <- lapply(data_by_state, rank_hospital, num)
  
  data.frame(hospital = unlist(pre_result), state = names(pre_result), row.names = names(pre_result))
}