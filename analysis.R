#### Analysis ####

setwd("~/Documents/Programming/ocha-chd")

library(ggplot2)
library(scales)
library(lubridate)
library(countrycode)

data <- read.csv('data/all-data.csv')

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
data.08 <- subset(data, year(data$created) == year(dmy("01-01-2008")))

# Ordering each year. 
data.13 <- data.13[order(data.13$created),]
data.12 <- data.12[order(data.12$created),]
data.11 <- data.11[order(data.11$created),]
data.10 <- data.10[order(data.10$created),]
data.09 <- data.09[order(data.09$created),]
data.08 <- data.08[order(data.08$created),]

# Sampling.
sample.13 <- rw.sample(df = data.13, n = 245)
sample.12 <- rw.sample(df = data.12, n = 245)
sample.11 <- rw.sample(df = data.11, n = 245)
sample.10 <- rw.sample(df = data.10, n = 245)
sample.09 <- rw.sample(df = data.09, n = 245)
sample.08 <- rw.sample(df = data.08, n = 245)

# Eliminating the 'country' column
sample.13$country <- NULL
sample.12$country <- NULL
sample.11$country <- NULL
sample.10$country <- NULL
sample.09$country <- NULL
sample.08$country <- NULL

# Writing the CSVs.
write.csv(sample.13, file = 'data/sample.13.csv', row.names = FALSE)
write.csv(sample.12, file = 'data/sample.12.csv', row.names = FALSE)
write.csv(sample.11, file = 'data/sample.11.csv', row.names = FALSE)
write.csv(sample.10, file = 'data/sample.10.csv', row.names = FALSE)
write.csv(sample.09, file = 'data/sample.09.csv', row.names = FALSE)
write.csv(sample.08, file = 'data/sample.08.csv', row.names = FALSE)


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

  
# Creating a frequency histogram. 
a <- data.frame(summary(data$iso3))
b <- data.frame(a[1:20,])
c <- data.frame(a[21:nrow(a), ])
total <- sum(c$summary.data.iso3.)
d <- data.frame(c(total))
d$iso3 <- "total"
colnames(d)[1] <- "summary.data.iso3."
b <- rbind(b,d)
b$country <- countrycode(toupper(b$iso3), "iso3c", "country.name")

k <- b$iso3
countrycode("COD", "iso3c", "country.name")

# Simple plot with the reliefweb data from 2013. #
ggplot(b, aes(reorder(b$iso3, b$summary.data.iso3.), b$summary.data.iso3.)) + theme_bw() + 
  geom_bar(stat = 'identity')




#### Bootstrapping #### 

collection <- read.csv('data/bootstrap/Data Collection Sheet - Luis.csv')

require(boot)

