rankall <- function(outcome, num = "best") {
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  data$State <- as.factor(data$State)
  
  possible_outcomes = c(11, 17, 23)
  names(possible_outcomes) <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% names(possible_outcomes))) {
    stop("invalid outcome")
  }
  outcome <- possible_outcomes[outcome]
  
  data <- data[, c(2, 7, outcome)]
  names(data) <- c("hospital", "state", "outcome")
  data$outcome <- as.numeric(data$outcome)
  
  data <- data[order(data$state, data$outcome, data$hospital), ]
  s <- split(data, data$state)
  l <- lapply(s, get_hospital_by_rank, num)
  d <- data.frame(hospital=unlist(l), state=names(l))
  
  d
}

get_hospital_by_rank <- function(data, num) {
  rank <- NA
  if (is.character(num)) {
    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      data <- data[!is.na(data$outcome), ]
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