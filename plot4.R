plot4 <- function(){
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
  data$DateTime <- strptime(paste(data$Date, data$Time),"%d/%m/%Y %H:%M:%S")
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
  par(mfrow=c(2,2))
  with(data, plot(DateTime, Global_active_power, type="l", ylab="Global Active Power", xlab =""))
  with(data, plot(DateTime, Voltage, type="l", ylab="Voltage", xlab ="datetime"))
  with(data, plot(DateTime, Sub_metering_1, type="n", ylab="Energy sub metering", xlab=""))
  lines(data$DateTime, data$Sub_metering_1, col="black")
  lines(data$DateTime, data$Sub_metering_2, col="red")
  lines(data$DateTime, data$Sub_metering_3, col="blue")
  legend("topright", lty=1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty="n")
  with(data, plot(DateTime, Global_reactive_power, type="l", ylab="Global Reactive Power", xlab ="datetime"))
} 
unzipDataset <- function(){
  unzip("exdata_data_household_power_consumption.zip")
}
deleteDataset <- function(){
  if (file.exists("household_power_consumption.txt")) {
    file.remove("household_power_consumption.txt")
  }
}
