#### Analysis ####

setwd("~/Documents/Programming/ocha-chd")

library(ggplot2)
library(scales)
library(lubridate)

data <- read.csv('data/all-entries.csv')

# Making the `created` column as.Date.
data$created <- as.Date(data$created)

# Ordering the whole dataset by date. 
data <- data[order(data$created),]

# Subsetting years. 
data.13 <- subset(data, year(data$created) == year(dmy("01-01-2013")))
data.12 <- subset(data, year(data$created) == year(dmy("01-01-2012")))
data.11 <- subset(data, year(data$created) == year(dmy("01-01-2011")))
data.10 <- subset(data, year(data$created) == year(dmy("01-01-2010")))
data.09 <- subset(data, year(data$created) == year(dmy("01-01-2009")))

# Ordering each year. 
data.13 <- data.13[order(data.13$created),]
data.12 <- data.12[order(data.12$created),]
data.11 <- data.11[order(data.11$created),]
data.10 <- data.10[order(data.10$created),]
data.09 <- data.09[order(data.09$created),]

# Sampling. 
sample.12 <- rw.sample(df = data.12, n = 220)
sample.11 <- rw.sample(df = data.11, n = 220)
sample.10 <- rw.sample(df = data.10, n = 220)
sample.09 <- rw.sample(df = data.09, n = 220)


##### Plotting ##### 
# Simple plot with all the Reliefweb data. #
ggplot(data, aes(created)) + theme_bw() + 
  geom_area(stat = 'bin', size = 1.3, fill = "#EB5C53", alpha = 0.3) + 
  geom_line(stat = 'bin', size = 1.3, color = "#EB5C53") + 
  scale_x_date(breaks = date_breaks("year"),
               labels = date_format("%Y"),
               limits = c(as.Date("1998-1-1"), as.Date("2014-2-28")))


# Simple plot with the reliefweb data from 2013. #
ggplot(data.13, aes(created)) + theme_bw() + 
  geom_area(stat = 'bin', size = 1.3, fill = "#0988bb", alpha = 0.3) + 
  geom_line(stat = 'bin', size = 1.3, color = "#0988bb") + 
  scale_x_date(breaks = date_breaks("year"),
               labels = date_format("%Y"),
               limits = c(as.Date("2013-1-1"), as.Date("2013-12-30")))

