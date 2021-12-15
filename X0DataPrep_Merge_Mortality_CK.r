library(tsModel) ; library(Epi) ; library(splines) ; 
library(lubridate) ; library(dplyr); library(dlnm); library(mgcv); library(vctrs)

setwd("D:/data/mortalityCHcities")

one <- readRDS("D:/data/mortalityCHcities/mortalityCH41-69.rds")
  
two <- readRDS("D:/data/mortalityCHcities/mortalityCH69-18.rds")


# Add the temperature mortality and the humidity data 
for(i in seq(length(one)) ) {
  data1 <- subset(one[[i]], year %in% c("1941":"1960")) # only have 20 years
  # <"1962")exclude the years 1962-1968 with only monthly data
  #data1 <- one[[i]]
  #data2 <- two[[i]]
  data2 <- subset(two[[i]], year %in% c("1999":"2018")) # only have 20 years

  
  # merge the data one (1941-1969) and two (1969-2018)
  y <- c(data1$year, data2$year)
  # m <- c(data1$month, data2$month)
  w <- c(data1$week, data2$week)
  d <- c(data1$deaths, data2$deaths)
  #m <- paste(data1, data2)
  
  # data frame
  data <- cbind(y,w,d)
  
  #set no entries in death rates to zero:
  #data3$deaths[is.na(data3$deaths)] <- 0
  
  # rename the columns  
  colnames(data) <- c("year", "week", "deaths")

  one[[i]] <- data
}
rm(data, data2)
# the resulting aggregation of cantonal data is saved into an rds-file
#saveRDS(fulldataset, file = "swiss_cantonal_89-18.rds")  
saveRDS(one, file = "mortalityCH1941-2018.rds")  

# # table kaspar staub
# 
# for(i in seq(length(one)) ) {
#   data1 <- subset(one[[i]], year<"1962") # <"1962")exclude the years 1962-1968 with only monthly data 
#   data2 <- two[[i]]
#   
#   
#   # merge the data one (1941-1969) and two (1969-2018)
#   y <- c(data1$year, data2$year_re)
#   m <- c(data1$month, data2$month)
#   w <- c(data1$week, data2$week)
#   d <- c(data1$deaths, data2$deaths)
#   #m <- paste(data1, data2)
#   
#   # data frame
#   data <- cbind(y,m,w,d)
#   
#   #set no entries in death rates to zero:
#   #data3$deaths[is.na(data3$deaths)] <- 0
#   
#   # rename the columns  
#   colnames(data) <- c("year", "month", "week", "deaths")
#   
#   one[[i]] <- data
# }
# rm(data, data2)
# 
# # summarize results:
# T1 <- matrix(NA, nrow=10, ncol=14)
# colnames(T1) <- c("40ies mean", "40ies IQR","50ies mean","50ies IQR", "70ies mean","70ies IQR","80ies mean","80ies IQR","90ies mean","90ies IQR","00s mean","00s IQR","10s mean","10s IQR")
# rownames(T1) <- names(one)
# for (j in seq(one)) {
# 
#   dta <- data.frame(one[[j]])
#   dta[is.na(dta)] <- 0
#   
#   
#   # COLLAPSE BY DECADE
#   # T1[j,1] <- quantile(dta$deaths[dta$year %in% c("1941":"1950")], probs=0.25)
#   # T1[j,2] <- quantile(dta$deaths[dta$year %in% c("1941":"1950")], probs= 0.50)
#   # T1[j,3] <- quantile(dta$deaths[dta$year %in% c("1941":"1950")], probs= 0.75)
#   T1[j,1] <- round(mean(dta$deaths[dta$year %in% c("1941":"1950")]), digits=2)
#   T1[j,2] <- round(IQR(dta$deaths[dta$year %in% c("1941":"1950")]), digits=2)
#   T1[j,3] <- round(mean(dta$deaths[dta$year %in% c("1951":"1960")]), digits=2)
#   T1[j,4] <- round(IQR(dta$deaths[dta$year %in% c("1951":"1960")]), digits=2)
#   T1[j,5] <- round(mean(dta$deaths[dta$year %in% c("1971":"1980")]), digits=2)
#   T1[j,6] <- round(IQR(dta$deaths[dta$year %in% c("1971":"1980")]), digits=2)
#   T1[j,7] <- round(mean(dta$deaths[dta$year %in% c("1981":"1990")]), digits=2)
#   T1[j,8] <- round(IQR(dta$deaths[dta$year %in% c("1981":"1990")]), digits=2)
#   T1[j,9] <- round(mean(dta$deaths[dta$year %in% c("1991":"2000")]), digits=2)
#   T1[j,10] <- round(IQR(dta$deaths[dta$year %in% c("1991":"2000")]), digits=2)
#   T1[j,11] <- round(mean(dta$deaths[dta$year %in% c("2001":"2010")]), digits=2)
#   T1[j,12] <- round(IQR(dta$deaths[dta$year %in% c("2001":"2010")]), digits=2)
#   T1[j,13] <- round(mean(dta$deaths[dta$year %in% c("2011":"2018")]), digits=2)
#   T1[j,14] <- round(IQR(dta$deaths[dta$year %in% c("2011":"2018")]), digits=2)
#   
# }
# 
# Table1 <- knitr::kable(T1, caption='Mean and IQR Weekly Mortality in Swiss Cities per Decade')
# write.xlsx(Table1, file = paste0(getwd(), "/Table1.xlsx", sep=""))
# 
# 
# plot(c(40,50,70,80,90,100,110), T1[1,c(1,3,5,7,9,11,13)], main="Basel Mortality Counts", ylab="Counts", xlab="decades")
#   lines(c(40,50,70,80,90,100,110), T1[2,c(1,3,5,7,9,11,13)], type="l")
#   legend("center", legend=c("mean", "iqr"), col="blue", "forestgreen", lty=1, cex=0.8)
