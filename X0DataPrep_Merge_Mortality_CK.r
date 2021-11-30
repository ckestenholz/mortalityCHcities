library(tsModel) ; library(Epi) ; library(splines) ; 
library(lubridate) ; library(dplyr); library(dlnm); library(mgcv); library(vctrs)

setwd("D:/data/mortalityCHcities")

one <- readRDS("D:/data/mortalityCHcities/mortalityCH41-69.rds")
  
two <- readRDS("D:/data/mortalityCHcities/mortalityCH69-18.rds")


# Add the temperature mortality and the humidity data 
for(i in seq(length(one)) ) {
  data1 <- subset(one[[i]], year<"1962") # exclude the years 1962-1968 with only monthly data
  #data1 <- one[[i]]
  data2 <- two[[i]]
  
  # merge the data one (1941-1969) and two (1969-2018)
  y <- c(data1$year, data2$year_re)
  m <- c(data1$month, data2$month)
  w <- c(data1$week, data2$week)
  d <- c(data1$deaths, data2$deaths)
  #m <- paste(data1, data2)
  
  # data frame
  data <- cbind(y,m,w,d)
  
  #set no entries in death rates to zero:
  #data3$deaths[is.na(data3$deaths)] <- 0
  
  # rename the columns  
  colnames(data) <- c("year", "month", "week", "deaths")

  one[[i]] <- data
}
rm(data, data2)
# the resulting aggregation of cantonal data is saved into an rds-file
#saveRDS(fulldataset, file = "swiss_cantonal_89-18.rds")  
saveRDS(one, file = "mortalityCH41-18.rds")  

