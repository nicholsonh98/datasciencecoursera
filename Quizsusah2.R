ubah <- function(x) {
    if (x < 10){
        paste(0, 0, x, sep="")
    }
    else if(x >= 10 & x < 100) {
        paste(0, x, sep="")
    }
    else {as.character(x)}
}

pollutantmean <- function(directory, pollutant, id = 1:332){
    totaldata <- data.frame()
    for (i in seq_along(id)) {
        data <- read.csv(paste(directory, "/", ubah(id[i]), ".csv", sep=""))
        totaldata <- rbind(totaldata, data)
    }
    if (pollutant == "sulfate"){
        mean(totaldata[,2], na.rm = TRUE)
    }
    else if (pollutant == "nitrate"){
        mean(totaldata[,3], na.rm = TRUE)
    }
    else {"Error!"}
}

complete <- function(directory, id = 1:332){
    data_complete = data.frame()
    for (i in seq_along(id)){
        data <- read.csv(paste(directory, "/", ubah(id[i]), ".csv", sep=""))
        idnum <- id[i]
        nobs <- sum(complete.cases(data))
        id_nobs <- data.frame()
        id_nobs <- cbind("id" = idnum, "nobs" = nobs)
        data_complete <- rbind(data_complete, id_nobs)
    }
    data_complete
}

corr <- function(directory, threshold = 0){
    vec_cor <- numeric()
    for (i in 1:332){
        data <- read.csv(paste(directory, "/", ubah(i), ".csv", sep=""))
        num_comp <- sum(complete.cases(data))
        if (num_comp >= threshold){
            data_sulfate <- data[,2][complete.cases(data)]
            data_nitrate <- data[,3][complete.cases(data)]
            vec_cor <- c(vec_cor, cor(data_sulfate, data_nitrate))
        }
        else {vec_cor <- vec_cor}
    }
    vec_cor
}