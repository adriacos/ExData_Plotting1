plot1 <- function(){
  #it wouldn't let me push the whole dataset to github otherwise
  unzipDataset()
  data <- readData()
  data <- mergeToDateTimes(data)
  data <- filterByDate(data)
  data <- convertToNumeric(data)
  plotData(data)
  #it wouldn't let me push the whole dataset to github otherwise 
  deleteDataset()
}

readData <- function(){
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE, colClasses="character")
}
mergeToDateTimes <- function(data){
  data$DateTime <- as.Date(paste(data$Date, data$Time),"%d/%m/%Y %H:%M:%S")
  data
}
filterByDate <- function(data){
  data <- subset(data, DateTime >= "2007-02-01" & DateTime < "2007-02-03")
  data
}
convertToNumeric <- function(data){
  data[3:9][data[3:9] == "?"] <- NA
  data[3:9] <- lapply(data[3:9], as.numeric)
  data
}
plotData <- function(data){
  hist(data$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
} 
unzipDataset <- function(){
  unzip("exdata_data_household_power_consumption.zip")
}
deleteDataset <- function(){
  if (file.exists("household_power_consumption.txt")) {
    file.remove("household_power_consumption.txt")
  }
}
