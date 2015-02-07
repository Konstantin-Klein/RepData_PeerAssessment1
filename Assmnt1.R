
library(dplyr)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

## Loading and preprocessing the data


if(!file.exists("activity.zip")) {
      print(noquote("downloading datafile from URL"))
      fileUrl <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(fileUrl, "activity.zip", method = "curl")
} else print(noquote("Datafile found"))

filename = "activity.csv"

if(!file.exists(filename)){
      print(noquote("No dataset found. Unzipping data from archive"))
      unzip("activity.zip")}

print(noquote("Reading data into R"))
activity <- read.csv("activity.csv", header = TRUE, sep = ',', quote = '\"',stringsAsFactors = FALSE)

activity$date <- as.Date(activity$date, "%Y-%m-%d")

activity <- tbl_df(activity)

# summarising data
print(noquote("Calculating annual emissions"))
totalByDate <- activity %>% group_by(date) %>% summarize(total_steps=sum(steps))

print(noquote("Building a plot"))
with(totalByDate, plot(date, total_steps, main = 'Total steps per day', ylab = 'Steps', xlab = 'Date'))
mean_steps <- mean(totalByDate$total_steps,na.rm = TRUE)
median_steps <- median(totalByDate$total_steps,na.rm = TRUE)      
abline(h=mean_steps, col = 'green')
abline(h=median_steps, col = 'blue')


