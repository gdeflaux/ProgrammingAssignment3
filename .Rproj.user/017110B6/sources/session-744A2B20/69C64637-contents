rankhospital <- function(state, outcome, num = "best") {
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
  data <- data[order(data$outcome, data$hospital), ]

  
  rank <- NA
  if (is.character(num)) {
    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- dim(data)[1]
    }
  } else if (is.numeric(num)) {
    if (num >= 1 & num <= dim(data)[1]) {
      rank <- num
    }
  }
  
  if (is.na(rank)) {
    return(rank)
  }

  data$hospital[rank]
}