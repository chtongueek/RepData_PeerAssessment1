#### Loading and preprocessing ####

graphics.off()
# 1) Read in data
data = read.csv("./activity.csv", header = T, stringsAsFactors = F)

# 2) Pre-processing: making number of steps a numeric
data$steps = as.numeric(data$steps)

#### What is the mean total number of steps taken per day ####

# 1) Calculate total number of steps per day
steps1 = aggregate(data$steps, by = list(data$date), FUN = sum, na.rm = T)

# 2) Make a histogram
hist(steps1[,2],10, main = "Steps Frequency", xlab = "Steps")

# 3) Calculate the mean and median number of steps for each day
steps2 = aggregate(data$steps, by = list(data$date), FUN = mean, na.rm = T)
steps3 = aggregate(data$steps, by = list(data$date), FUN = median, na.rm = T)

totals = cbind(steps2, steps3[,2])

names(totals) = c("Date", "Average Steps", "Median Steps")

#### What is the average daily activity pattern ####
# Get the mean number of steps per interval
pat = aggregate(data$steps, by = list(data$interval), FUN = mean, na.rm = T)

# 1) Plot the mean number of steps per interval
plot(pat, type = 'l', main = "Average Steps", xlab = "Interval", ylab = "Steps")

# 2) Get the interval with the maximum mean number of steps
maxInt = pat[max(pat[,2]), 1]

#### Imputing missing values ####

# 1) Count the total number of NAs
NAcount = sum(is.na(data))

# 2) & 3)
# Create second data frame equal to the first
data2 = data

# Find where NAs occur
subs = which(is.na(data2[,1]))

# replace NAs with mean corresponding to their interval tag
for (i in 1:length(subs)){
  
  data2[subs[i],1] = pat[data2$interval[i] == pat[,1], 2]
  
}

# 4) Find the total, mean, and median number of steps per interval
rep1 = aggregate(data2$steps, by = list(data2$date), FUN = sum, na.rm = T)
rep2 = aggregate(data2$steps, by = list(data2$date), FUN = mean, na.rm = T)
rep3 = aggregate(data2$steps, by = list(data2$date), FUN = median, na.rm = T)

hist(rep1[,2], 10, main = "Step Frequency (NA replaced)", xlab = "Steps")

#### Differences in activity patterns ####

## Date 1, 2012-10-01 is a Monday ##

# 1)
# Convert date to date type
data2$date = as.Date(data2$date)

# Take the difference of each date and the first date, mod 7
datediff = as.numeric((data2$date - data2$date[1]))%%7

# The first date is a monday, so datediffs from 0-4 will be monday thru friday
# datediffs 5 and 6 will be saturday and sunday
data2$wkdy[datediff %in% 0:4] = "weekday"
data2$wkdy[datediff %in% 5:6] = "weekend"

wkdy = aggregate(data2$steps, by = list(data2$interval, data2$wkdy), FUN = mean)
names(wkdy) = c("interval", "weekday", "steps")

# 2)
library(lattice)
xyplot(wkdy$steps~wkdy$interval | wkdy$weekday, layout = c(1,2), type = 'l', xlab = "Interval", ylab = "Average Steps")
