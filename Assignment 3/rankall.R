rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (outcome != "heart attack" & outcome != "heart failure" &
             outcome != "pneumonia") stop("invalid outcome")
    ## For each state, find the hospital of the given rank
    else {
        hospnames <- character()
        for (state in sort(unique(data$State))){
            datastate <- data[data$State == state, ]
            if (outcome == "heart attack") column <- as.numeric(datastate[,11])
            else if (outcome == "heart failure") column <- as.numeric(datastate[,17])
            else column <- as.numeric(datastate[,23])
            hosp <- lookuphosp(column, datastate, num)
            names(hosp) <- state
            hospnames <- c(hospnames, hosp)
        }
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    dataout <- cbind(hospital = hospnames, state = sort(unique(data$State)))
    data.frame(dataout)
}

lookuphosp <- function(column, data, num){
    sorthosbyname <- data[,2][order(data[,2])]
    sortcolbyname <- column[order(data[,2])]
    sorthosbycol <- sorthosbyname[order(sortcolbyname, na.last = NA)]
    max.index <- length(sorthosbycol)
    num <- as.character(num)
    hosp <- {
        if (num == "best") sorthosbycol[1]
        else if (num == "worst") sorthosbycol[max.index]
        else if (as.numeric(num) > max.index) NA
        else sorthosbycol[as.numeric(num)]
    }
    hosp
}

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
