rankhospital <- function(state, outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if(!any(data$State == state)) stop("invalid state")
    else if (outcome != "heart attack" & outcome != "heart failure" &
             outcome != "pneumonia") stop("invalid outcome")
    ## Return hospital name in that state with lowest 30-day death
    else {
        datastate <- data[data$State == state, ]
        lookuphosp(outcome, datastate, num)
    }
}

lookuphosp <- function(outcome, data, num){
    if (outcome == "heart attack") column <- as.numeric(data[,11])
    else if (outcome == "heart failure") column <- as.numeric(data[,17])
    else column <- as.numeric(data[,23])
    sorthosbyname <- data[,2][order(data[,2])]
    sortcolbyname <- column[order(data[,2])]
    sorthosbyval <- sorthosbyname[order(sortcolbyname, na.last = NA)]
    max.index <- length(sorthosbyval)
    num <- as.character(num)
    if (num == "best") sorthosbyval[1]
    else if (num == "worst") sorthosbyval[max.index]
    else if (as.numeric(num) > max.index) NA
    else sorthosbyval[as.numeric(num)]
}
