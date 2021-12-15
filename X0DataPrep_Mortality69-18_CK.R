# mortality data 1969-2018: data aggregation

# library(magrittr) # needs to be run every time you start R and want to use %>%
# don't load this otherwise the pipes do not work anymore


library(dplyr)    # alternatively, this also loads %>%
library(tidyverse)
library(ISOweek)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(magrittr)

setwd("D:/data/mortalityCHcities")

# SET DIRECTORY OF DATA INPUT
dir <- "D:/data/mortalityCHcities/Raw"

# LOAD DATA
munciptable <- readRDS(paste0(dir,"/municipalitylookuptable.rds"))
deathrecords <- readRDS(paste0(dir,"/deathrecordsmuncipality.rds"))

# SAVE ONLY DEATHS of 2018
#deathrecords <- deathrecords[(deathrecords$yy=="2018"),]
#deathrecords <- deathrecords[!(deathrecords$yy<"1989"),]

# # CREATE AGE CATE3GORY
# deathrecords$age_cat <- NA
# deathrecords$age_cat <- deathrecords$age_death
# deathrecords$age_cat = ifelse(deathrecords$age_cat<=64,"1", ifelse(deathrecords$age_cat<=79,"2", ifelse(deathrecords$age_cat<=150, "3")))


# and use in the pipes, or the cplyr package, "group_by(date,kantonname)

# CREATE LOOKUP TABLE
lookuptable <- deathrecords %>%
  group_by(NAME,comm_resi, assigned_number,Kantonname) %>%
  summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))
# 
# # CREATE LOOKUP TABLE
# lookuptable <- deathrecords %>%
#   group_by(NAME,comm_resi, assigned_number,Kantonname) %>%
#   summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))# 

# X <- deathrecords %>%
#   group_by(date, Kantonname) %>%
#   summarise(date=date, KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))

# Y <- deathrecords %>%
#   group_by(KANTONSNUM, Kantonname, date) %>%
#   summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))# 

Z <- deathrecords %>%
  group_by(comm_resi, assigned_number, NAME, date) %>%
  summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))# 


# Z <- deathrecords %>%
#   group_by(NAME,comm_resi, assigned_number,Kantonname) %>%
#   summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))


Basel <- subset(Z, comm_resi=="2701")
Bern <- subset(Z, comm_resi=="351") 
Biel <- subset(Z, comm_resi=="371")
LaChauxdeFonds <- subset(Z, comm_resi=="6421")
Geneva <- subset(Z, comm_resi=="6621")
Lausanne <- subset(Z, comm_resi=="5586")
Lucerne <- subset(Z, comm_resi=="1061")
StGallen <- subset(Z, comm_resi=="3203")
Winterthur <- subset(Z, comm_resi=="230")
Zurich <- subset(Z, comm_resi=="261")

# Time: Specifiy range of data
date1 <- as.Date("1969/1/1") 
# 29.12.1969 bis 04.01.1970 -> KW01, 1969
# Mo 29, Di. 30, Mi 31, Do, 01, Fr. 02, Sa. 03, So. 04
date2 <- as.Date("2018/12/30") # End W52 2018
# date2 <- as.Date("1969/1/5") # End W52 2018

#date2 <- as.Date("2018/12/31") # which week


# FILL IN MISSING DATE VALUES AND DEAH RATES (na)

Basel %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Bern %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Biel %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

LaChauxdeFonds %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Geneva %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Lausanne %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Lucerne %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

StGallen %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Winterthur %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

Zurich %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))





stations <- list(Basel, Bern, Biel, LaChauxdeFonds, Geneva, Lausanne, Lucerne, StGallen, Winterthur, Zurich)
names(stations) <- c("Basel", "Bern", "Biel", "La Chaux-de-Fonds", "Geneva", "Lausanne", "Lucerne", "St. Gallen", "Winterthur", "Zurich")



# Time: Add time variables

for(i in seq(length(stations))) {
  data <- stations[[i]]
  # PRINT
  cat(i,"")
  data$date <- ymd(data$date)
  data$year <- year(data$date)
  data$month <- month(data$date)
  data$week <- isoweek(data$date)
  data$day <- day(data$date)
  data$yday <- yday(data$date)
  data$dow <- wday(data$date)
  stations[[i]] <- data
}
rm(data)


# statyion <- stations
# 
# # Collapse Daily in Weekly data:
# berna <- berna2 <- berna3 <- stations[[2]]
# 
# time <- seq(from =ymd(date1),to= ymd(date2+1), by="days")
# test1 <-  berna %>% group_by(year, week, NAME) %>%  
#   # data.w <- data %>% group_by(year, month, week, NAME) %>% 
#   summarize(deaths=sum(deaths)) %>%
#   arrange(year, week)
# 
# test2 <-  berna2 %>% group_by(year, month, week, NAME) %>%  
#   # data.w <- data %>% group_by(year, month, week, NAME) %>% 
#   summarize(deaths=sum(deaths)) %>%
#   arrange(month, week)
# 
# test3 <-  berna3 %>% group_by(year, month, week, NAME) %>%  
#   # data.w <- data %>% group_by(year, month, week, NAME) %>% 
#   summarize(deaths=sum(deaths)) %>%
#   arrange(year, month, week)
#  note: problem when collapsing by week - week 1, 53 and 52 might fall in two years, so when I collapse by year week if can be split which is not correct.
#  we need to select the year for all the week that corresponds to the last year included.

# for(i in seq(length(stations))) {
#   data <- stations[[i]]
#   # PRINT
#   data$year_re <- rep(NA, nrow(data))
#   for (j in seq (1969,2018)){
#     data$year_re[data$week==52 & data$year==j & data$month==1] <- j-1
#     data$year_re[data$week==1 & data$year==j & data$month==12] <- j+1
#   }
#   data$year_re[is.na(data$year_re)] <- data$year[is.na(data$year_re)]
#   stations[[i]] <- data
# }
# rm(data)  

for(i in seq(length(stations))) {
  data <- stations[[i]]  
  # SET TO 0 THE NA
  data$deaths[is.na(data$deaths)] <- 0
  data.w <-  data %>% group_by(year, week, NAME) %>%  
    # data.w <- data %>% group_by(year, month, week, NAME) %>% 
    summarize(deaths=sum(deaths)) %>%
    arrange(year, week)
  # exclusion week 53
  data.w52 <- subset(data.w, week!=53)
  stations[[i]] <- data.w52
}
rm(data, data.w52, data.w)


# the resulting aggregation of cantonal mortality data is saved into an rds-file
saveRDS(stations, file = "mortalityCH69-18.rds")
