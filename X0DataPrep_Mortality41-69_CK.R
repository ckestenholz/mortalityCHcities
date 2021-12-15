###############################################################################
# Mortality Swiss Cities Data Preparation # 23.11.2021 - Christoph Kestenholz #
###############################################################################

# mortality data 1941-1969: data aggregation

# Define working directory
#setwd("Y:/CCH/02Projects")
setwd("D:/data/mortalityCHcities")

# Add libraries
library("plyr")
library("data.table")
library("dplyr")
library("seas")
library("zoo") # for date manipulation
library("lubridate")
library("ISOweek")
library("xlsx")

# Read Raw data
# X_total <- read.xlsx("D:/data/mortalityCHcities/Raw/Data1941-69.xlsx", 1, header = TRUE) # total residents + non residents
X <- read.xlsx("D:/data/mortalityCHcities/Raw/Data1941-69.xlsx", 2, header = TRUE) # residents
# X_NONresi <- read.xlsx("D:/data/mortalityCHcities/Raw/Data1941-69.xlsx", 3, header = TRUE) # non-residents
# X_pop <- read.xlsx("D:/data/mortalityCHcities/Raw/Data1941-69.xlsx", 4, header = TRUE) # population size

# Prepare time
# X$week <- isoweek(X$StartReportingPeriod)
X$week <- isoweek(X$EndReportingPeriod)

# X$year <- year(X$StartReportingPeriod)
X$year <- year(X$EndReportingPeriod)

# X$month <- month(X$StartReportingPeriod)
X$month <- month(X$EndReportingPeriod)




# Assign Missing Values as "NA": e.g.: "-"
X$Basel[(X$Basel == "")] <- NA
X$Bern[(X$Bern == "")] <- NA
X$Biel[(X$Biel == "")] <- NA
X$LaChauxdeFonds[(X$LaChauxdeFonds == "")] <- NA
X$Geneve[(X$Geneve == "")] <- NA
X$Lausanne[(X$Lausanne == "")] <- NA
X$Luzern[(X$Luzern == "")] <- NA
X$St.Gallen[(X$St.Gallen == "")] <- NA
X$Winterthur[(X$Winterthur == "")] <- NA
X$Zürich[(X$Zürich == "")] <- NA

# remove the columns with no information
X <- X[ , !names(X) %in% c("NA.","NA..1")] 

# Precise the column names
colnames(X)[4:13] <- c("Basel", "Bern", "Biel", "La Chaux-de-Fonds", "Geneva", "Lausanne", "Lucerne", "St. Gallen", "Winterthur", "Zurich")


Basel <- X[, c("year", "month", "week", "Basel", "StartReportingPeriod", "EndReportingPeriod")]
names(Basel) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Bern <- X[, c("year", "month", "week", "Bern", "StartReportingPeriod", "EndReportingPeriod")]
names(Bern) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Biel <- X[, c("year", "month", "week", "Biel", "StartReportingPeriod", "EndReportingPeriod")]
names(Biel) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
LaChauxdeFonds <- X[, c("year", "month", "week", "La Chaux-de-Fonds", "StartReportingPeriod", "EndReportingPeriod")]
names(LaChauxdeFonds) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Geneva <- X[, c("year", "month", "week", "Geneva", "StartReportingPeriod", "EndReportingPeriod")]
names(Geneva) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Lausanne <- X[, c("year", "month", "week", "Lausanne", "StartReportingPeriod", "EndReportingPeriod")]
names(Lausanne) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Lucerne <- X[, c("year", "month", "week", "Lucerne", "StartReportingPeriod", "EndReportingPeriod")]
names(Lucerne) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
StGallen <- X[, c("year", "month", "week", "St. Gallen", "StartReportingPeriod", "EndReportingPeriod")]
names(StGallen) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Winterthur <- X[, c("year", "month", "week", "Winterthur", "StartReportingPeriod", "EndReportingPeriod")]
names(Winterthur) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")
Zurich <- X[, c("year", "month", "week", "Zurich", "StartReportingPeriod", "EndReportingPeriod")]
names(Zurich) <- c("year", "month", "week", "deaths", "StartReportingPeriod", "EndReportingPeriod")


stations <- list(Basel, Bern, Biel, LaChauxdeFonds, Geneva, Lausanne, Lucerne, StGallen, Winterthur, Zurich)
names(stations) <- c("Basel", "Bern", "Biel", "La Chaux-de-Fonds", "Geneva", "Lausanne", "Lucerne", "St. Gallen", "Winterthur", "Zurich")

# # rename the deaths as deaths
# for(i in seq(length(stations))) {
#   data <- stations[[i]]
#   # PRINT
#   cat(i,"")
#   data$date <- ymd(data$date)
#   data$year <- year(data$date)
#   data$month <- month(data$date)
#   data$week <- isoweek(data$date)
#   data$day <- day(data$date)
#   data$yday <- yday(data$date)
#   data$dow <- wday(data$date)
#   stations[[i]] <- data
# }
# rm(data)

# # Time: Specifiy range of data
# date1 <- as.Date("1941/1/1")
# date2 <- as.Date("1969/12/31")
# 
# for(i in seq(length(stations))) {
#   data <- stations[[i]]
#   # PRINT
#   data %<>%
#     mutate(week = as.Date(week)) %>%
#     complete(week = seq.Date(date1, date2, by="week"))
#   
#   stations[[i]] <- data
# }
# rm(data)



for(i in seq(length(stations))) {
  data <- stations[[i]]  
  # exclusion week 53
  data.w52 <- subset(data, week!=53)
  stations[[i]] <- data.w52
}
rm(data, data.w52)


# the resulting aggregation of cantonal mortality data is saved into an rds-file
saveRDS(stations, file = "mortalityCH41-69.rds")


# write.table(X, file = "DataMortality_41-69_CK.txt", sep = ",", quote = FALSE, row.names = T)



#######
# END #
#######