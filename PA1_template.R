#REPRODUCIBLE RESEARCH - ASSIGNMENT 1
####################################

##Initials (private)
setwd("C:/Users/LeonardoF/Dropbox/Desenvolvimento/Coursera - Data Science Specialization (Johns Hopkins)/Geral/R wd/Assignments/5- Reproducible Research")
library(graphics)
library(ggplot2)
library(knitr)

##LOADING AND PREPROCESSING THE DATA
### Loading data
cDataDirectory = "./Data"
cFileName <- "activity.csv"
myData <- read.table (file = paste(cDataDirectory, cFileName, sep = "/"),
                      header = TRUE,
                      sep = ",")
rm(cDataDirectory, cFileName)
FilterNA <- !is.na(myData$steps)          

### Understanding NA values
DayWithNAstepsAux1 <- xtabs(is.na(steps) ~ interval + date,
                            data = myData)     
DayWithNAstepsAux2 <- colSums(DayWithNAstepsAux1)
DayWithNAsteps <- DayWithNAstepsAux2[DayWithNAstepsAux2>0]
AllNAinFullNADays <- mean(DayWithNAsteps == (24*12)) ==1

rm(DayWithNAstepsAux1, DayWithNAstepsAux2, DayWithNAsteps)



##WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY

### Mean total number of steps per day, considering NA = zero
DailySum1 <- xtabs(steps ~ date,
                   data = myData)     

hist(x = DailySum1, 
     main = "Steps per day (assumption: NA = interval mean)", 
     breaks = c(0, 3000, 6000, 9000, 12000, 15000, 18000, 21000, 24000), 
     xlab = "Quantity of daily steps", 
     xlim = c(0, 24000), 
     xaxp = c(0, 24000, 8), 
     cex.axis = 0.8, 
     ylab = "Day frequency", 
     ylim = c(0,30), 
     yaxt = "n", 
     labels = TRUE, 
     col = "gray")

MeanDailySteps1 <- mean(DailySum1)
MedianDailySteps1 <- median(x = DailySum1)


##What is the average daily activity pattern?
IntervalSum1 <- xtabs(steps ~ interval, 
                     data = myData)

NValidDates <- length(DailySum1[DailySum1>0])   #interval mean only within non NA values
IntervalMean1 <- IntervalSum1 / NValidDates

plot(x = names(IntervalMean1),
     y = as.numeric(IntervalMean1), 
     type = "l", 
     main = "Steps per interval", 
     xlab = "Interval",
     ylab = "Quantity of steps", 
     las = 1, 
     col = "blue", 
     lwd = 2)

MaxIntervalSteps <- names(IntervalMean1[IntervalMean1 == max(IntervalMean1)])
MaxSteps <- formatC(x = max(IntervalMean1), format = "d")


##Inputing missing values
### Calculate and report the total number of missing values in the dataset
CountNA <- sum(is.na(myData$steps))

### Devise a strategy for filling in all of the missing values in the dataset...
### strategy: mean for that 5-minute interval
StepsWithoutNA <- vector(mode = "numeric", length = nrow(myData))

StepsWithoutNA <- vector(mode = "numeric", length = nrow(myData))
StepsWithoutNA[FilterNA] <- myData$steps[FilterNA]
StepsWithoutNA[!FilterNA] <- IntervalMean1[as.character(myData$interval[!FilterNA])]

### Create a new dataset that is equal to the original dataset but with the missing data filled in.
myNewData <- myData
myNewData$steps <- StepsWithoutNA

### Make a histogram of the total number of steps taken each day
DailySum2 <- xtabs(steps ~ date,
                   data = myNewData)     
hist(x = DailySum2, 
     main = "Steps per day", 
     breaks = c(0, 3000, 6000, 9000, 12000, 15000, 18000, 21000, 24000), 
     xlab = "Quantity of daily steps", 
     xlim = c(0, 24000), 
     xaxp = c(0, 24000, 8), 
     cex.axis = 0.8, 
     ylab = "Day frequency", 
     ylim = c(0,30), 
     yaxt = "n", 
     labels = TRUE, 
     col = "gray")

### Calculate and report the mean and median total number of steps taken per day
MeanDailySteps2 <- mean(DailySum2)
MedianDailySteps2 <- median(x = DailySum2)


#Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels
WeekDay <- weekdays(as.Date(myNewData$date), abbreviate = TRUE)
DayType <- vector(mode = "character", length = length(WeekDay))
DayType[WeekDay %in% c("dom", "sáb")] <- "Weekend"
DayType[WeekDay %in% c("seg", "ter", "qua", "qui", "sex")] <- "Weekday"
myNewData <- cbind(myNewData, DayType)

## Make a panel plot containing a time series plot...
### Data
FiltroWDay <- DayType == "Weekday"
IntervalSumWDay <- xtabs(steps ~ interval, 
                      data = myNewData, 
                      subset = FiltroWDay)
NValidDatesWDay <- length(IntervalSumWDay[FiltroWDay==TRUE]) / (24*12)
IntervalMeanWDay <- IntervalSumWDay / NValidDatesWDay
IntervalSumWEnd <- xtabs(steps ~ interval, 
                         data = myNewData, 
                         subset = !FiltroWDay)
NValidDatesWEnd <- length(IntervalSumWDay[FiltroWDay==FALSE]) / (24*12)
IntervalMeanWEnd <- IntervalSumWEnd / NValidDatesWEnd

PanelPlot <- data.frame(interval = as.numeric(names(IntervalMeanWDay)), 
                       Weekday = as.numeric(IntervalMeanWDay),
                       Weekend = as.numeric(IntervalMeanWEnd))



dataPlot1 <- data.frame(interval = as.numeric(names(IntervalMeanWDay)), daytype = "weekday", steps = as.numeric(IntervalMeanWDay))
dataPlot2 <- data.frame(interval = as.numeric(names(IntervalMeanWEnd)), daytype = "weekend", steps = as.numeric(IntervalMeanWEnd))
dataPlot <- rbind(dataPlot1, dataPlot2)

head(dataPlot)
summary(dataPlot)


### Panel plot
par(mfrow = c(1,2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0), cex = 0.8, cex.axis = 0.8)
with(PanelPlot, {
      plot(interval, Weekday, main = "Weekday", type = "l", ylab = "steps", cex.main = 0.9, col = "blue")
      plot(interval, Weekend, main = "Weekend", type = "l", ylab = "steps", cex.main = 0.9, col = "red")
      mtext("Steps per interval", outer = TRUE)
})

### Unified plot
par(mfrow = c(1,1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0), cex = 0.8, cex.axis = 0.8)
with(dataPlot, plot(interval, steps, 
                    main = "Steps per interval", cex.main = 0.9,
                    type = "n", 
                    ylab = "steps", xlab = "interval"))
with(subset(dataPlot, daytype == "weekday"), points(interval, steps, type = "l", lwd = 2, col = "blue"))
with(subset(dataPlot, daytype == "weekend"), points(interval, steps, type = "l", lwd = 2, col = "red"))
legend("topright", pch = 15, col = c("blue", "red"), legend = c("weekday", "weekend"))