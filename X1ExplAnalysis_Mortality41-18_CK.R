# Excess Mortality for Cantonal Data - Christoph Kestenholz

# working directory
setwd("D:/data/mortalityCHcities")

# set language to english
Sys.setenv(LANG = "en")

## Define a function to run the study at once:
run <- function(x){

    
data <- readRDS("D:/data/mortalityCHcities/mortalityCH41-18.rds")

# import libraries
library(tidyverse)
library(ISOweek)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(ggpubr)
library(cowplot)
library(qpdf)
library(xlsx)

# import color palette 
col_reg <- c(brewer.pal(12, "Paired"), "black")
col_reg[11] <- "Khaki4" # instead of yellow


# # SELECT SPECIFIC CANTON TO RUN THE STUDY
# cant <- "Zuerich"
# # cant <- "Geneve"
# # cant <- "Valais"
# # cant <- "Ticino" 
# # cant <- "Basel-Stadt"
# 
# for (cant in c("Zuerich", "Geneve", "Valais", "Ticino", "Basel-Stadt")) {
# 
# print("-----------------------------------------------")
# print("The selected canton for the Excess Mortality is")
# print(paste0(cant))
# print("-----------------------------------------------")
# 
# # SELECT SERIES AFTER 1999
# if (cant == "Zuerich") {
#   dta <- subset(data$Zuerich, year>"1999")
# } else {
#   if (cant == "Geneve") {
#     dta <- subset(data$Geneve, year>"1999")
#   } else {
#     if (cant == "Valais") {
#       dta <- subset(data$Valais, year>"1999")
#     } else {
#       if (cant == "Ticino") {
#         dta <- subset(data$Ticino, year> "1999")
#       } else {
#         if (cant == "Basel-Stadt") {
#           dta <- subset(data$`Basel-Stadt`, year> "1999")
#         } else {
#         print("No canton selected!")
#         }
#       }
#     }
#   }
# }

dta <- data.frame(data[[10]])

# SET TO 0 THE NA
dta[is.na(dta)] <- 0


#####################################
# Mortality prediction with a model #
#####################################

# CREATE PARAMETER FOR TIME TRENDS
trend <- as.numeric(seq(1:dim(dta)[1])) ## create trend variable, begin with 1
trend1 <- as.numeric(seq(1:1096)) ## create trend variable, 1941-1962
trend2 <- as.numeric(seq(1097:dim(dta)[1])) ## create trend variable, 1969-2018


# CREATE A SEASONAL COMPONENT
sin.t <- sin(2*pi*trend/52.143) # weekly
sin.t1 <- sin(2*pi*trend1/52.143) # weekly 1941-1962
sin.t2 <- sin(2*pi*trend2/52.143) # weekly 1969-2018

cos.t <- cos(2*pi*trend/52.143) # weekly
cos.t1 <- cos(2*pi*trend1/52.143) # weekly 1941-1962
cos.t2 <- cos(2*pi*trend2/52.143) # weekly 1969-2018

# sin.t <- sin(2*pi*trend/365) # daily
# cos.t <- cos(2*pi*trend/365) # daily
# plot(trend, cos.t)

# MODEL 
model <- glm(deaths ~ trend + sin.t + cos.t , family="poisson", data=dta)
model1 <- glm(deaths[1:1096] ~ trend1 + sin.t1 + cos.t1 , family="poisson", data=dta)
model2 <- glm(deaths[1097:dim(dta)[1]] ~ trend2 + sin.t2 + cos.t2 , family="poisson", data=dta)


#  CREATE PREDICTION DATASET
data.pred <- data.frame(cbind(trend, sin.t, cos.t))
data.pred1 <- data.frame(cbind(trend1, sin.t1, cos.t1))
data.pred2 <- data.frame(cbind(trend2, sin.t2, cos.t2))


pred <- predict(model, data.pred, type="response", se.fit=TRUE)
pred1 <- predict(model1, data.pred1, type="response", se.fit=TRUE)
pred2 <- predict(model2, data.pred2, type="response", se.fit=TRUE)


dta.pred.deaths <- data.frame(deaths_pred = pred$fit, date= dta$week)
dta.pred.deaths1 <- data.frame(deaths_pred1 = pred1$fit, date= dta$week[1:1096])
dta.pred.deaths2 <- data.frame(deaths_pred2 = pred2$fit, date= dta$week[1097:dim(dta)[1]])


# TEST PLOT
mort_pred_obs <- ggplot() + 
  geom_line(data=dta, aes(x=seq(dta$week), y=dta$deaths), color=col_reg[6], size=.3) +
  geom_line(data=dta.pred.deaths, aes(x=seq(dta.pred.deaths$date), y=dta.pred.deaths$deaths_pred), color=col_reg[2], size=0.5) +
  geom_line(data=dta.pred.deaths1, aes(x=seq(1:1096), y=dta.pred.deaths1$deaths_pred), color=col_reg[4], size=0.5) +
  geom_line(data=dta.pred.deaths2, aes(x=seq(1097:dim(dta)[1]), y=dta.pred.deaths2$deaths_pred), color=col_reg[8], size=0.5) +
  
  theme(
    legend.position = "right",
    panel.border = element_blank(),
  ) +
  theme_minimal() +
  ggtitle(paste0("Observed and predicted weekly all-cause mortality: 2000-2018", "\n", cant)) +
  scale_x_continuous(name="years", breaks=yearli, labels=yearli_t) +
  scale_y_continuous(name="deaths", limits=c(mean(data_pred.w$deaths)-40, mean(data_pred.w$deaths)+40))
mort_pred_obs



# #  COLLAPSE BY  WEEK & YEAR
# #  note: problem when collapsing by week - week 1, 53 and 52 might fall in two years, so when I collapse by year week if can be split which is not correct.
# #  we need to select the year for all the week that corresponds to the last year included.
# dta$week <- isoweek(dta$date)
# 
# # PRED
# data.pred$week <- dta$week
# 
# data.pred$year <- year(dta$date)
# data.pred$month <- month(dta$date)
# 
# dta$year <- year(dta$date)
# dta$month <- month(dta$date)
# 
# dta$year_re <- rep(NA, nrow(dta))
# for (i in seq(2000,2018)){
#   dta$year_re[dta$week==52 & dta$year==i  & dta$month==1] <- i-1
#   dta$year_re[dta$week==1 & dta$year==i  & dta$month==12] <- i+1
# }
# dta$year_re[is.na(dta$year_re)] <- dta$year[is.na(dta$year_re)]
# 
# 
# data.pred$year_re <- rep(NA, nrow(data.pred))
# for (i in seq(2000,2018)){
#   data.pred$year_re[data.pred$week==52 & data.pred$year==i  & data.pred$month==1] <- i-1
#   data.pred$year_re[data.pred$week==1 & data.pred$year==i  & data.pred$month==12] <- i+1
# }
# data.pred$year_re[is.na(data.pred$year_re)] <- data.pred$year[is.na(data.pred$year_re)]
# 
# data.pred$deaths <- dta.pred.deaths$deaths_pred
# data.pred$date <- dta.pred.deaths$date
# 
# # EXCLUSIONS
# dta <- subset(dta, week!=53)
# dta <- subset(dta, year_re!=2019)
# 
# data.pred <- subset(data.pred, week!=53)
# data.pred <- subset(data.pred, year_re!=2019)
# 
# # COLLAPSE BY WEEK:YEAR
# dta.w <- dta %>% group_by(week,year_re) %>% 
#                           summarize(deaths=sum(deaths)) %>%
#                           arrange(year_re, week)
# 
# data_pred.w <- data.pred %>% group_by(week,year_re) %>% 
#                           summarize(deaths=sum(deaths)) %>%
#                           arrange(year_re,week)
# summary(dta.w)
# summary(data_pred.w)
# 
# # PLOT All-cause PREDICTED and OBSERVED mortailty aggregated by week in Switzerland for 2000-2018
# indyear <- (dta.w$week==1)+1
# yearli <- c(which(indyear==2))
# yearli_t <- as.character(c(2000:2018))
# mort_pred_obs <- ggplot() + 
#   geom_line(data=dta.w, aes(x=seq(dta.w$week), y=dta.w$deaths), color=col_reg[6], size=.3) +
#   geom_line(data=data_pred.w, aes(x=seq(data_pred.w$week), y=data_pred.w$deaths), color=col_reg[2], size=0.5) +
#   theme(
#     legend.position = "right",
#     panel.border = element_blank(),
#   ) +
#   theme_minimal() +
#   ggtitle(paste0("Observed and predicted weekly all-cause mortality: 2000-2018", "\n", cant)) +
#   scale_x_continuous(name="years", breaks=yearli, labels=yearli_t) +
#   scale_y_continuous(name="deaths", limits=c(mean(data_pred.w$deaths)-40, mean(data_pred.w$deaths)+40))
# mort_pred_obs
# ggsave(paste0(getwd(), "/img/", cant, "_mort_pred_obs", ".pdf", sep=""), device = "pdf", width = 15)
# 
# 
# 
# ####################################
# #####Analysis for one year only ####
# ####################################
# # EXCLUSIONS
# dta.w2000 <- subset(dta.w, year_re==2000)
# dta.w2001 <- subset(dta.w, year_re==2001)
# dta.w2002 <- subset(dta.w, year_re==2002)
# dta.w2003 <- subset(dta.w, year_re==2003)
# dta.w2004 <- subset(dta.w, year_re==2004)
# dta.w2005 <- subset(dta.w, year_re==2005)
# dta.w2006 <- subset(dta.w, year_re==2006)
# dta.w2007 <- subset(dta.w, year_re==2007)
# dta.w2008 <- subset(dta.w, year_re==2008)
# dta.w2009 <- subset(dta.w, year_re==2009)
# dta.w2010 <- subset(dta.w, year_re==2010)
# dta.w2011 <- subset(dta.w, year_re==2011)
# dta.w2012 <- subset(dta.w, year_re==2012)
# dta.w2013 <- subset(dta.w, year_re==2013)
# dta.w2014 <- subset(dta.w, year_re==2014)
# dta.w2015 <- subset(dta.w, year_re==2015)
# dta.w2016 <- subset(dta.w, year_re==2016)
# dta.w2017 <- subset(dta.w, year_re==2017)
# dta.w2018 <- subset(dta.w, year_re==2018)
# 
# # calculate the average deaths over 2010-2018:
# dta.waver <- dta.w %>% group_by(week) %>% 
#           summarise(deaths=mean(deaths)) %>%
#           arrange(week)
# 
# # PLOT All years All-cause observed mortailty aggregated by week in Switzerland for 2000-2018
# weekli <- seq(1, 52, by=4.333333333)
# weekli_t <- c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# mort_obs <- ggplot() +
#   ggtitle(paste0("Observed weekly all-cause mortality: Summers 2000-2018 ", "\n", cant, sep=" ")) +
#   theme_minimal() +
#   geom_line(data=dta.w2003, aes(x=seq(week), y=deaths, colour="2003"), size=.3) +
#   geom_line(data=dta.w2015, aes(x=seq(week), y=deaths, colour="2015"), size=.3) +
#   geom_line(data=dta.w2018, aes(x=seq(week), y=deaths, colour="2018"), size=.3) +
#   geom_line(data=dta.waver, aes(x=seq(week), y=deaths, colour="average for all years 2000-2018"), size=0.7) +
#   scale_x_continuous(name= "year", breaks=weekli, labels=weekli_t, limits=c(weekli[5]-2, weekli[10]+2)) +
#   ylab("weekly deaths") +
#   #scale_y_continuous(name = "weekly deaths") +
#   ylim(.9*min(dta.w2003$deaths), 1.1*max(dta.w2003$deaths)) +
#   #scale_y_continuous(name = "weekly deaths", limits = c(150, 260)) +
#   geom_vline(xintercept = weekli[[5]], linetype="dotted", 
#              colour = "black", size=.5) +
#   geom_vline(xintercept = weekli[[10]], linetype="dotted", 
#             color = "black", size=.5) +
#   theme(legend.title = element_blank())+
#   theme(legend.position="top")
# plot(mort_obs)
# ggsave(paste0(getwd(), "/img/", cant, "_mort_obs",".pdf", sep=""), device = "pdf", width = 10)
# 
# # SELECT SPECIFIC SUMMER TO RUN THE STUDY
# # yr <- "2003"
# # yr <- "2015"
# # yr <- "2018"
# 
# 
# for (yr in c("2003", "2015", "2018")) {
# 
# print("-----------------------------------------------")
# print("The selected year for the Excess Mortality is")
# print(paste0(yr))
# print("-----------------------------------------------")
# 
# # SELECT SERIES AFTER 1999
# if (yr == "2003") {
#   dta_yr <- subset(dta, week!=53)
#   dta_yr <- subset(dta, year_re==2003&month %in% c(5:9))
#   data.pred_yr <- subset(data.pred, week!=53)
#   data.pred_yr <- subset(data.pred, year_re==2003&month %in% c(5:9))
# } else {
#   if (yr == "2015") {
#     dta_yr <- subset(dta, week!=53)
#     dta_yr <- subset(dta, year_re==2015&month %in% c(5:9))
#     data.pred_yr <- subset(data.pred, week!=53)
#     data.pred_yr <- subset(data.pred, year_re==2015&month %in% c(5:9))
#   } else {
#     if (yr == "2018") {
#       dta_yr <- subset(dta, week!=53)
#       dta_yr <- subset(dta, year_re==2018&month %in% c(5:9))
#       data.pred_yr <- subset(data.pred, week!=53)
#       data.pred_yr <- subset(data.pred, year_re==2018&month %in% c(5:9))
#     } else {
#       print("No year was selected!")
#     }
#   }
# }
# 
# # Calculate percentage of excess-mortality
# dta_yr$excess <- ((dta_yr$deaths - data.pred_yr$deaths) / data.pred_yr$deaths)*100
# dta_yr$posexcess <- dta_yr$excess; dta_yr$posexcess[dta_yr$posexcess<0] <-0
# dta_yr$negexcess <- dta_yr$excess; dta_yr$negexcess[dta_yr$negexcess>0] <-0
# 
# # DEFINITION EXCESS-MORTALITY: Vicedo SMW 2016:
# ## "The percentage excess mortality corresponds to the number 
# ## of excess deaths divided by the number of expected deaths."
# 
# # restrict the dummy (tempy,humy,mort) variable for a hot and humid day to 2018:
# if (yr == "2003") {
#   dta_yr$dummy <- dta_summer$dummy[dta_summer$year==2003]
#   dta_yr$temy <- dta_summer$temy[dta_summer$year==2003]
#   dta_yr$humy <- dta_summer$humy[dta_summer$year==2003]
#   dta_yr$mort <- dta_summer$mort[dta_summer$year==2003]
#   
# } else {
#   if (yr == "2015") {
#     dta_yr$dummy <- dta_summer$dummy[dta_summer$year==2015]
#     dta_yr$temy <- dta_summer$temy[dta_summer$year==2015]
#     dta_yr$humy <- dta_summer$humy[dta_summer$year==2015]
#     dta_yr$mort <- dta_summer$mort[dta_summer$year==2015]
#     
#   } else {
#     if (yr == "2018") {
#       dta_yr$dummy <- dta_summer$dummy[dta_summer$year==2018]
#       dta_yr$temy <- dta_summer$temy[dta_summer$year==2018]
#       dta_yr$humy <- dta_summer$humy[dta_summer$year==2018]
#       dta_yr$mort <- dta_summer$mort[dta_summer$year==2018]
#     } else {
#       print("No year was selected!")
#     }
#   }
# }
# 
# # Exceedances of 90th percentile for the selected year:
# dta_yr$tex <- dta_yr$`max temperature [deg]`
# dta_yr$tex[dta_yr$tex < limit_temp] <- NA
# 
# dta_yr$hex <- dta_yr$`partial pressure [hPa]`
# dta_yr$hex[dta_yr$hex < limit_humi] <- NA
# 
# dta_yr$mex <- dta_yr$deaths
# dta_yr$mex[dta_yr$mex < limit_mort] <- NA
# 
# ####################
# # Evaluate Results #
# ####################
# 
# # compound event deaths per day:
# dta_yr$d_compound <- 0 # set to zero for non-event
# dta_yr$d_compound[dta_yr$dummy>0] <- 1 # set to 1 for event
# dta_yr$d_compound_lag <- dta_yr$d_compound
# 
# for (a in seq(1:150)) {
#   if (dta_yr$d_compound[a] == 1){
#     dta_yr$d_compound_lag[a] <- 1
#     dta_yr$d_compound_lag[a+1] <- 1
#     dta_yr$d_compound_lag[a+2] <- 1
#     dta_yr$d_compound_lag[a+3] <- 1
#   } else {
#   }
# }
# # calculate daily mortality (rate and percentage) for a compound day:
# RES_RATE[paste0(cant, "-", yr, sep=""),1] <- sum(dta_yr$deaths * dta_yr$d_compound_lag) / sum(dta_yr$d_compound_lag)
# RES_PERC[paste0(cant, "-", yr, sep=""),1] <- sum(dta_yr$excess * dta_yr$d_compound_lag) / sum(dta_yr$d_compound_lag)
# 
# # hot event deaths per day:
# dta_yr$d_hot <- 0
# dta_yr$d_hot[dta_yr$temy>0] <- 1
# dta_yr$d_hot_lag <- dta_yr$d_hot
# 
# for (a in seq(1:150)) {
#   if (dta_yr$d_hot[a] == 1){
#     dta_yr$d_hot_lag[a] <- 1
#     dta_yr$d_hot_lag[a+1] <- 1
#     dta_yr$d_hot_lag[a+2] <- 1
#     dta_yr$d_hot_lag[a+3] <- 1
#   } else {
#   }
# }
# # calculate daily mortality for a hot day:
# RES_RATE[paste0(cant,"-",yr, sep=""),2] <- sum(dta_yr$deaths * dta_yr$d_hot_lag) / sum(dta_yr$d_hot_lag)
# RES_PERC[paste0(cant,"-",yr, sep=""),2] <- sum(dta_yr$excess * dta_yr$d_hot_lag) / sum(dta_yr$d_hot_lag)
# 
# # humid event deaths per day:
# dta_yr$d_humid <- 0
# dta_yr$d_humid[dta_yr$humy>0] <- 1
# dta_yr$d_humid_lag <- dta_yr$d_humid
# 
# for (a in seq(1:150)) {
#   if (dta_yr$d_humid[a] == 1){
#     dta_yr$d_humid_lag[a] <- 1
#     dta_yr$d_humid_lag[a+1] <- 1
#     dta_yr$d_humid_lag[a+2] <- 1
#     dta_yr$d_humid_lag[a+3] <- 1
#   } else {
#   }
# }
# # calculate daily mortality for a humid day:
# RES_RATE[paste0(cant,"-",yr, sep=""),3] <- sum(dta_yr$deaths * dta_yr$d_humid_lag) / sum(dta_yr$d_humid_lag) # rate
# RES_PERC[paste0(cant,"-",yr, sep=""),3] <- sum(dta_yr$excess * dta_yr$d_humid_lag) / sum(dta_yr$d_humid_lag) # rate
# 
# # calculate average daily mortality for a summer day:
# RES_RATE[paste0(cant,"-",yr, sep=""),4] <- sum(dta_yr$deaths) / length(dta_yr$date) # rate
# RES_PERC[paste0(cant,"-",yr, sep=""),4] <- sum(dta_yr$excess) / length(dta_yr$date) # percentage
# 
# # calculate % increased rate compound-average:
# RES_RATE[paste0(cant,"-",yr, sep=""), 5] <- (RES_RATE[paste0(cant,"-",yr, sep=""),1] * 100 / RES_RATE[paste0(cant,"-",yr, sep=""),4]) - 100 # rate
# RES_PERC[paste0(cant,"-",yr, sep=""), 5] <- RES_PERC[paste0(cant,"-",yr, sep=""),1] - RES_PERC[paste0(cant,"-",yr, sep=""),4] # percentage
# 
# # PLOT histogram for exceedances for temperature and humidity for the selected summer:
# hist_tex_yr <- ggplot(dta_yr, aes(x=tex)) + 
#   geom_histogram(color=col_reg[2], fill=col_reg[1]) +
#   ggtitle(paste0("90th percentile exceedances for daily maximum temperature: Summer ", yr, "\n", cant, sep=" ")) +
#   xlab("maximum temperature [deg]") +
#   ylab("counts") +
#   theme_minimal()
# hist_tex_yr
# ggsave(paste0(getwd(), "/img/", cant, "_", yr, "_hist_tex", ".pdf", sep=""), device = "pdf")
# 
# hist_hex_yr <- ggplot(dta_yr, aes(hex)) + 
#   geom_histogram(color=col_reg[4], fill=col_reg[3]) +
#   ggtitle(paste0("90th percentile exceedances for daily mean partial pressure: Summer ", yr, "\n", cant, sep=" ")) +
#   xlab("partial pressure [hPa]") +
#   ylab("counts") +
#   theme_minimal()
# hist_hex_yr
# ggsave(paste0(getwd(), "/img/", cant, "_", yr, "_hist_hex", ".pdf", sep=""), device = "pdf")
# 
# hist_mex_yr <- ggplot(dta_yr, aes(mex)) + 
#   geom_histogram(color=col_reg[6], fill=col_reg[5]) +
#   ggtitle(paste0("90th percentile exceedances for daily deaths: Summer ", yr, "\n", cant, sep=" ")) +
#   xlab("deaths") +
#   ylab("counts") +
#   theme_minimal()
# hist_mex_yr
# ggsave(paste0(getwd(), "/img/", cant, "_", yr, "_hist_mex", ".pdf", sep=""), device = "pdf")
# 
# # PLOT Excess Mortality for the selected summer:
# dummy_factor <- max(dta_yr$posexcess)+15
# summer <- c(min(dta_yr$date),min(dta_yr$date)+31,min(dta_yr$date)+61,min(dta_yr$date)+92,min(dta_yr$date)+123,min(dta_yr$date)+153)
# summer_t <- c("May","Jun","Jul","Aug","Sep", "Oct")
# 
# ex_mort_yr <- ggplot(data=dta_yr) +
#   geom_segment( aes(x=date, xend=date, y=-0*d_compound_lag, yend=dummy_factor*d_compound_lag), color=col_reg[8], size=2.0, alpha=0.001) +
#   geom_segment( aes(x=date, xend=date, y=-d_hot_lag*0, yend=(dummy_factor - 10)*d_hot_lag), color=col_reg[7], size=2.0, alpha=0.3) +
#   geom_segment( aes(x=date, xend=date, y=-d_humid_lag*0, yend=(dummy_factor - 10)*d_humid_lag), color=col_reg[1], size=2.0, alpha=0.3) +
#   geom_segment( aes(x=date, xend=date, y=0, yend=posexcess), color=col_reg[6], size=1.35) +
#   geom_line( aes(x=date, y=`max temperature [deg]`+50),  color =col_reg[8],  size=.5) +
#   geom_line( aes(x=date, y=`partial pressure [hPa]`+50), color =col_reg[2],  size=.5) +
#   theme_minimal() +
#   ggtitle(paste0("All-cause daily mortality: Summer ", yr, "\n", cant, sep=" ")) +
#   scale_x_continuous(name= "months", breaks=summer, labels=summer_t, limits=c(min(summer), max(summer))) +
#   scale_y_continuous(name = "percentage of excess mortality", 
#                    sec.axis = sec_axis(~.-50, name = "maximum temperature (\u00B0C) and humidity as mean partial pressure (hPa)", breaks=c(0,10,20,30,40))) 
# ex_mort_yr
# 
# # prepare a legend seperately:
# legend_ex_mort_yr <- ggplot(data=dta_yr) +
#   geom_segment( aes(x=date, xend=date, y=-0*d_compound_lag*dummy_factor, yend=dummy*dummy_factor, color="Compound Hot-Humid Day"), size=1.35, alpha=0.1) +
#   geom_segment( aes(x=date, xend=date, y=-d_hot_lag*0, yend=limit_temp*d_hot_lag, color="Hot Day"),size=1.35, alpha=0.3) +
#   geom_segment( aes(x=date, xend=date, y=-d_humid_lag*0, yend=limit_humi*d_humid_lag, color="Humid Day"),size=1.35, alpha=0.3) +
#   geom_segment( aes(x=date, xend=date, y=0, yend=posexcess, color="Positive Excess Mortality"), size=1.35) +
#   geom_line( aes(x=date, y=`max temperature [deg]`, color="Daily Maxmimum Temperature"), size=.9) +
#   geom_line( aes(x=date, y=`partial pressure [hPa]`, color="Daily Mean Partial Pressure"),  size=.9) +
#   scale_color_manual(values = c("gray80", col_reg[7], col_reg[1], 
#                                 col_reg[6], col_reg[8],
#                                 col_reg[2]),
#                     labels = c("Compound Hot-Humid day", "Hot Day (>= 90th% Tmax)", "Humid Day (>= 90th% PaPr)",
#                               "Positive Excess Mortality", "Daily Maximum Temperature",
#                               "Daily Mean Partial Pressure")) +
#   scale_linetype_manual(values = c('solid', 'solid', 'solid', 
#                                    'solid', 'solid', 
#                                    'solid')) +
#   theme_minimal() +
#   theme(legend.title = element_blank())+
#   theme(legend.position="right")
# #legend_ex_mort_yr
# 
# 
# # extract the legend
# legend_only <- as_ggplot(get_legend(legend_ex_mort_yr))
# #legend_only
# 
# # merge the plot w the legend via cowplot
# #ex_mort_yr <- plot_grid(ex_mort_yr, NULL, legend_only, NULL, nrow= 1, rel_widths= c(1, 0.18, 0.2, 0.18))
# ex_mort_yr <- plot_grid(ex_mort_yr, NULL, legend_only, NULL, nrow= 1, rel_widths= c(1, 0.001, 0.2, 0.01))
# ex_mort_yr
# #test <- plot_grid(e_m_summer_yr, legend_only, ncol=1, rel_heights = c(1,0.2))
# 
# ggsave(paste0(getwd(), "/img/", cant, "_", yr, "_ex_mort", ".pdf", sep=""), device = "pdf", width=13)
# 
# 
# 
# } # yr loop
# 
# #dev.off()
# 
# } # cant loop
# 
# write.xlsx(RES_RATE, file = paste0(getwd(), "/output/RES_RATE.xlsx", sep=""), sheetName = "death_rates", 
#            col.names = TRUE, row.names = TRUE, append = FALSE)
# write.xlsx(RES_PERC, file = paste0(getwd(), "/output/RES_RATE.xlsx", sep=""), sheetName = "death_percentages", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)
# 
# return("Done")
# }
# 
# ##################
# # CONCATENATE PDF#
# ##################
# 
# # update working directory
# setwd("D:/data/TempHumidity/01/img/")
# 
# # Mortality (Mortality Observation, Mortality Prediction, Excess Mortalities 2003 - 2015 - 2018)
# pdf_combine(c("Zuerich_mort_obs.pdf", "Zuerich_mort_pred_obs.pdf", "Zuerich_2003_ex_mort.pdf", "Zuerich_2015_ex_mort.pdf", "Zuerich_2018_ex_mort.pdf"), "Zuerich/Zuerich_Mortality.pdf")
# pdf_combine(c("Geneve_mort_obs.pdf", "Geneve_mort_pred_obs.pdf", "Geneve_2003_ex_mort.pdf", "Geneve_2015_ex_mort.pdf", "Geneve_2018_ex_mort.pdf"), "Geneve/Geneve_Mortality.pdf")
# pdf_combine(c("Valais_mort_obs.pdf", "Valais_mort_pred_obs.pdf", "Valais_2003_ex_mort.pdf", "Valais_2015_ex_mort.pdf", "Valais_2018_ex_mort.pdf"), "Valais/Valais_Mortality.pdf")
# pdf_combine(c("Ticino_mort_obs.pdf", "Ticino_mort_pred_obs.pdf", "Ticino_2003_ex_mort.pdf", "Ticino_2015_ex_mort.pdf", "Ticino_2018_ex_mort.pdf"), "Ticino/Ticino_Mortality.pdf")
# pdf_combine(c("Basel-Stadt_mort_obs.pdf", "Basel-Stadt_mort_pred_obs.pdf", "Basel-Stadt_2003_ex_mort.pdf", "Basel-Stadt_2015_ex_mort.pdf", "Basel-Stadt_2018_ex_mort.pdf"), "Basel-Stadt/Basel-Stadt_Mortality.pdf")
# 
# # Histograms (90th% Exceedances of overall and yearli Tmax, Partial Pressure, Daily Mortality)
# pdf_combine(c("Zuerich_hist_tex.pdf", "Zuerich_hist_hex.pdf",
#               "Zuerich_2003_hist_tex.pdf", "Zuerich_2003_hist_hex.pdf", "Zuerich_2003_hist_mex.pdf",
#               "Zuerich_2015_hist_tex.pdf", "Zuerich_2015_hist_hex.pdf", "Zuerich_2015_hist_mex.pdf",
#               "Zuerich_2018_hist_tex.pdf", "Zuerich_2018_hist_hex.pdf", "Zuerich_2018_hist_mex.pdf"),
#             "Zuerich/Zuerich_Histograms.pdf")
# pdf_combine(c("Geneve_hist_tex.pdf", "Geneve_hist_hex.pdf",
#               "Geneve_2003_hist_tex.pdf", "Geneve_2003_hist_hex.pdf", "Geneve_2003_hist_mex.pdf",
#               "Geneve_2015_hist_tex.pdf", "Geneve_2015_hist_hex.pdf", "Geneve_2015_hist_mex.pdf",
#               "Geneve_2018_hist_tex.pdf", "Geneve_2018_hist_hex.pdf", "Geneve_2018_hist_mex.pdf"),
#             "Geneve/Geneve_Histograms.pdf")
# pdf_combine(c("Valais_hist_tex.pdf", "Valais_hist_hex.pdf",
#               "Valais_2003_hist_tex.pdf", "Valais_2003_hist_hex.pdf", "Valais_2003_hist_mex.pdf",
#               "Valais_2015_hist_tex.pdf", "Valais_2015_hist_hex.pdf", "Valais_2015_hist_mex.pdf",
#               "Valais_2018_hist_tex.pdf", "Valais_2018_hist_hex.pdf", "Valais_2018_hist_mex.pdf"),
#             "Valais/Valais_Histograms.pdf")
# pdf_combine(c("Ticino_hist_tex.pdf", "Ticino_hist_hex.pdf",
#               "Ticino_2003_hist_tex.pdf", "Ticino_2003_hist_hex.pdf", "Ticino_2003_hist_mex.pdf",
#               "Ticino_2015_hist_tex.pdf", "Ticino_2015_hist_hex.pdf", "Ticino_2015_hist_mex.pdf",
#               "Ticino_2018_hist_tex.pdf", "Ticino_2018_hist_hex.pdf", "Ticino_2018_hist_mex.pdf"),
#             "Ticino/Ticino_Histograms.pdf")
# pdf_combine(c("Basel-Stadt_hist_tex.pdf", "Basel-Stadt_hist_hex.pdf",
#               "Basel-Stadt_2003_hist_tex.pdf", "Basel-Stadt_2003_hist_hex.pdf", "Basel-Stadt_2003_hist_mex.pdf",
#               "Basel-Stadt_2015_hist_tex.pdf", "Basel-Stadt_2015_hist_hex.pdf", "Basel-Stadt_2015_hist_mex.pdf",
#               "Basel-Stadt_2018_hist_tex.pdf", "Basel-Stadt_2018_hist_hex.pdf", "Basel-Stadt_2018_hist_mex.pdf"),
#             "Basel-Stadt/Basel-Stadt_Histograms.pdf")

#######
# END #
#######