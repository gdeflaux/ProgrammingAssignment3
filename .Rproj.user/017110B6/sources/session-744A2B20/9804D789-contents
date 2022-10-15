best <- function(state, outcome) {
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  data$State <- as.factor(data$State)
  
  possible_outcomes = c(11, 17, 23)
  names(possible_outcomes) <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% names(possible_outcomes))) {
    stop("invalid outcome")
  }
  outcome <- possible_outcomes[outcome]
  
  data <- data[, c(2, 7, outcome)]
  names(data) <- c("hospital", "state", "outcome")
  data$outcome <- as.numeric(data$outcome)
  
  data <- data[data$state == state & !is.na(data$outcome), ]
  min_value <- min(data$outcome)
  data <- data[data$outcome == min_value, ]
  
  sort(data$hospital)[1]
}