best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if(!any(data$State == state)) stop("invalid state")
    else if (outcome != "heart attack" & outcome != "heart failure" &
             outcome != "pneumonia") stop("invalid outcome")
    ## Return hospital name in that state with lowest 30-day death
    else {
        datastate <- data[data$State == state, ]
        lookupminhos(outcome, datastate)
    }
}

lookupminhos <- function(outcome, data){
    if (outcome == "heart attack") column <- as.numeric(data[,11])
    else if (outcome == "heart failure") column <- as.numeric(data[,17])
    else column <- as.numeric(data[,23])
    sortedhos <- data[,2][order(data[,2])]
    sortedcol <- column[order(data[,2])]
    indices <- which(sortedcol == min(sortedcol, na.rm = TRUE))
    sortedhos[indices[1]]
}
