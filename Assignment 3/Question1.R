outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "Death Rates from Heart Attack",
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack")
