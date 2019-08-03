# Opening File hw1
filename <- file("hw1_data.csv", "r")
# readdata is data.frame
readdata <- read.csv(filename)
readdata[1:2,] # view first two row
readdata[47,1] # value of ozone in 47th row
nrow(readdata) # total number of row in the data
ncol(readdata) # total number of column in the data
length(na.omit(readdata[,1])) # number of data without the NA in Ozone
sum(complete.cases(readdata)) # complete case -> NA will return FALSE, numeric will return TRUE, the sum would be the number of numeric value (without NA)
# complete.cases(readdata) == na.omit(readdata)
nrow(readdata)-length(na.omit(readdata[,1])) # number of NA in Ozone
sum(is.na(readdata[,1])) # number of NA in Ozone, because NA will return TRUE or 1, sum of them will be number of NA

mean(na.omit(readdata[,1])) # SIMPLE WAY OF GETTING MEAN IN OZONE WITHOUT NA, or you can do the following:
########################################################################################
# bad is vector, checking readdata[,1] Ozone is NA... return True kalo NA
bad <- is.na(readdata[,1])
# get clear ozone, readdata[,1] Ozone if False
get_clear_ozone <- readdata[,1][!bad]
sprintf("The mean of ozone: %f", mean(get_clear_ozone))
sprintf("This is the mean of ozone too: %f", mean(readdata[,1][!is.na(readdata[,1])]))
########################################################################################


### Extracting the mean of Solar.R where Ozone values are above 31 and Temp values are above 90

# Copy readdata to readdata2
readdata2 <- readdata
# Make Ozone below 32 NA
i=1
for(val in readdata2[,1]){
  if(val < 32 || is.na(val)) readdata2[i,1] = NA
  i = i + 1
}
# Make Temp below 91 NA
i=1
for(val in readdata2[,4]){
  if(val < 91 || is.na(val)) readdata2[i,4] = NA
  i = i + 1
}
# Use complete cases to take no NA
good <- complete.cases(readdata2)
# Print data frame
print("Cleaned data (readdata2): ")
print.data.frame(readdata2[good,]) # Print data frame
print("Solar.R in cleaned data: ")
print.simple.list(readdata2[good,2]) # Print simple list
sprintf("This is the mean of Solar.R where Ozone values are above 31 and Temp values are above 90: %f", mean(readdata[good,2]))

### Extracting the mean of Solar.R where Ozone values are above 31 and Temp values are above 90 SECOND WAY
# Pake logical vector index x[x>0]... hmm.. kayaknya gabisa...

### What is the mean of "Temp" when "Month" is equal to 6?
readdata3 <- readdata
temp_month_data <- data.frame("Temp" = numeric(0), "Month" = numeric(0)) # Make zero data frame
i = 1 # for looping
for(val in readdata3[,5]){
  if (val == 6) temp_month_data <- rbind(temp_month_data, readdata3[i,4:5])
  i = i + 1
}
mean(temp_month_data[,1])

### What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
###############################################################################
ozone_month_data <- data.frame("Ozone" = numeric(0), "Month" = numeric(0))
i = 1
for(val in readdata3[,5]){
  if (val == 5) ozone_month_data <- rbind(ozone_month_data, readdata3[i,c(1,5)])
  i = i + 1
}
max(ozone_month_data[,1], na.rm=TRUE)
### INI SAMA KAYAK BLOCK BAWAH ###
ozone_month_5 <- numeric(0)
i = 1
for(val in readdata3[,5]){
  if (val == 5 && !is.na(readdata3[i,1])) ozone_month_5 <- c(ozone_month_5, readdata3[i,1])
  i = i + 1
}
max(ozone_month_5)
##############################