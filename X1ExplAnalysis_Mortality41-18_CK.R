# Excess Mortality for Cantonal Data - Christoph Kestenholz

# working directory
setwd("D:/data/mortalityCHcities")

# set language to english
Sys.setenv(LANG = "en")

## Define a function to run the study at once:
run <- function(x){

 
# data <- readRDS("D:/data/mortalityCHcities/old/mortalityCH41-18.rds") #   old
data <- readRDS("D:/data/mortalityCHcities/mortalityCH1941-2018.rds") # new
city <- names(data) # save the city names

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

# summarize results:
RES_RATE <- matrix(NA, nrow=10, ncol=8)
colnames(RES_RATE) <- c("1947: Hot Week", "1947: Average Week", "2003: Hot Week", "2003: Average Week","2015: Hot Week", "2015: Average Week","2018: Hot Week", "2018: Average Week")
rownames(RES_RATE) <- names(data)
RES_PERC <- RES_RATE

for (i in seq(data)) {
citta <- city[i]
dta <- data.frame(data[[i]])

# SET TO 0 THE NA
#dta[is.na(dta)] <- 0 # not possible since some are NA


# HEATWAVE EVENTS
# summarize results:
# T1 <- matrix(NA, nrow=10, ncol=2)
# colnames(T1) <- c("90P", "EVENTS WEEK")
# rownames(T1) <- names(data)
# # for (j in seq(data)) {
# 
# # dta <- data.frame(data[[j]])
# # dta[is.na(dta)] <- 0
# 
# 
# T1[1,1] <- 30.3
# T1[1,2] <- list(c(22, 23, 30:32, 33, 34, 37:39)) # heatwave weeks for basel
# T1[2,1] <- 29.3
# T1[2,2] <- c(30:32, 33, 34) # heatwave weeks for bern
# T1[5,1] <- 30.9
# T1[5,2] <- c(30:32) # heatwave weeks for geneva
# T1[7,1] <- 29.3
# T1[7,2] <- c(30:32) # heatwave weeks for lucerne
# T1[10,1] <- 28.9
# T1[10,2] <- c(30:32, 33, 34, 37:39) # heatwave weeks for zurich



#}


# # exclusion week 53
# dta <- subset(dta, week!=53)

# # test subset
# test1 <- subset(dta, year %in% c("1941":"1950")) # 
# test2 <- subset(dta, year %in% c("1951":"1960")) # 
# test3 <- subset(dta, year %in% c("1961":"1970")) # more 50+ weeks
# test4 <- subset(dta, year %in% c("1971":"1981")) # no data available
# test5 <- subset(dta, year %in% c("1981":"1900")) # 
# test6 <- subset(dta, year %in% c("1991":"2000")) # unequal number of weeks
# test7 <- subset(dta, year %in% c("2001":"2010")) # unequal number of weeks
# test8 <- subset(dta, year %in% c("2011":"2018")) # unequal number of weeks

# sum of week 53
# test1: 2
# test2: 2
# test3: 0

# test4: 0 (no data)
# test5: 4
# test6: 2
# test7: 4
# test8: 2




dta1 <- subset(dta, year %in% c("1941":"1960")) # 1941 - 1961
dta2 <- subset(dta, year %in% c("1999":"2018")) # 1998 - 2018


########### PROBLEM #############
#################################
# i should end up with only 52 weeks per year! 
# now i sometimes have two times week 1
# and also week 53
# aggregate the deaths or delete
# end up with equal amount of weeks for the dta1 and dta2

# EXCLUSIONS
dta1 <- subset(dta1, week!=53)
dta2 <- subset(dta2, week!=53)

########################################
# Mortality prediction with two models #
########################################


# CREATE PARAMETER FOR TIME TRENDS
trend1 <- as.numeric(seq(1:nrow(dta1))) ## create trend variable, begin with 1
trend2 <- as.numeric(seq(1:nrow(dta2))) ## create trend variable, begin with 1

# CREATE A SEASONAL COMPONENT
sin.t1 <- sin(2*pi*trend1/52) # weekly 1941-1961 nweek: 52.143
sin.t2 <- sin(2*pi*trend2/52) # 52.143

cos.t1 <- cos(2*pi*trend1/52) # weekly 1941-1962
cos.t2 <- cos(2*pi*trend2/52) # weekly 1998-2018

# sin.t <- sin(2*pi*trend/365) # daily
# cos.t <- cos(2*pi*trend/365) # daily
# plot(trend, cos.t)

# MODEL 

model1 <- glm(deaths ~ trend1 + sin.t1 + cos.t1 , family="poisson", data=dta1)
model2 <- glm(deaths ~ trend2 + sin.t2 + cos.t2 , family="poisson", data=dta2)


#  CREATE PREDICTION DATASET
data.pred1 <- data.frame(cbind(trend1, sin.t1, cos.t1))
data.pred2 <- data.frame(cbind(trend2, sin.t2, cos.t2))


pred1 <- predict(model1, data.pred1, type="response", se.fit=TRUE)
pred2 <- predict(model2, data.pred2, type="response", se.fit=TRUE)


dta.pred.deaths1 <- data.frame(deaths_pred1 = pred1$fit, year=dta1$year, week= dta1$week)
dta.pred.deaths2 <- data.frame(deaths_pred2 = pred2$fit, year=dta2$year, week= dta2$week)


# PLOT Time Series 1: 1941 - 1961
yearli <- seq(from=1, to=nrow(dta1), by=52)
yearli_t <- as.character(c(1941:1960))
    
mort_pred_obs1 <- ggplot() + 
  geom_line(data=dta1, aes(x=seq(dta1$week), y=dta1$deaths), color=col_reg[6], size=.3) +
  geom_line(data=dta.pred.deaths1, aes(x=seq(dta1$week),  y=dta.pred.deaths1$deaths_pred), color=col_reg[4], size=0.5) +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
  ) +
  theme_minimal() +
  ggtitle(paste0("Observed and predicted weekly all-cause mortality: 1941-1960", "\n", citta)) +
  scale_x_continuous(name="weeks (1941-1960)", breaks=yearli, labels=yearli_t) +
  scale_y_continuous(name="deaths") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
mort_pred_obs1
ggsave(paste0(getwd(), "/img/", citta, "_mort_pred_obs-41-60", ".pdf", sep=""), device = "pdf", width = 15)


# PLOT Time Series 2: 1998 - 2018
yearli <- seq(from=1, to=nrow(dta2), by=52)
yearli_t <- as.character(c(1999:2018))

mort_pred_obs2 <- ggplot() + 
  geom_line(data=dta2, aes(x=seq(dta2$week), y=dta2$deaths), color=col_reg[6], size=.3) +
  geom_line(data=dta.pred.deaths2, aes(x=seq(dta2$week),  y=dta.pred.deaths2$deaths_pred), color=col_reg[4], size=0.5) +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
  ) +
  theme_minimal() +
  ggtitle(paste0("Observed and predicted weekly all-cause mortality: 1999-2018", "\n", citta)) +
  scale_x_continuous(name="weeks (1999-2018)", breaks=yearli, labels=yearli_t) +
  scale_y_continuous(name="deaths") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
mort_pred_obs2
ggsave(paste0(getwd(), "/img/", citta, "_mort_pred_obs-99-18", ".pdf", sep=""), device = "pdf", width = 15)

# # PLOT Time Series 2: 1941 - 2018
# yearli <- seq(from=1, to=nrow(dta), by=62.5238)
# yearli_t <- as.character(c(1941:1961,1969:2018))
# 
# mort_pred_obs_f <- ggplot() + 
#   geom_line(data=dta1, aes(x=seq(dta$week), y=dta$deaths), color=col_reg[6], size=.3) +
#   theme(
#     legend.position = "right",
#     panel.border = element_blank(),
#   ) +
#   theme_minimal() +
#   ggtitle(paste0("Observed and predicted weekly all-cause mortality: 1941-2018", "\n", citta)) +
#   scale_x_continuous(name="weeks (1941-2018)", breaks=yearli, labels=yearli_t) +
#   scale_y_continuous(name="deaths") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# mort_pred_obs_f
# ggsave(paste0(getwd(), "/img/", citta, "_mort_pred_obs-full", ".pdf", sep=""), device = "pdf", width = 15)


####################################
#####Analysis for one year only ####
####################################

# SELECT SPECIFIC SUMMER TO RUN THE STUDY

# yr <- "1947"
# yr <- "2003"
# yr <- "2015"
# yr <- "2018"
# 
# 
for (yr in c("1947", "2003", "2015", "2018")) {

print("-----------------------------------------------")
print("The selected year for the Excess Mortality is")
print(paste0(yr))
print("-----------------------------------------------")

# SELECT SERIES AFTER 1999
if (yr == "1947") {
  #dta_yr <- subset(dta1, week!=53)
  dta_yr <- subset(dta1, year==1947&week %in% c(19:39))
  #dta_yr <- subset(dta1, year==1947&month %in% c(5:9))
  #data.pred_yr <- subset(data.pred1, week!=53)
  data.pred_yr <- subset(dta.pred.deaths1, year==1947&week %in% c(19:39))
  #data.pred_yr <- subset(data.pred1, year==1947&month %in% c(5:9))
  hwDAYS <- c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0)
} else {
  if (yr == "2003") {
    #dta_yr <- subset(dta2, week!=53)
    dta_yr <- subset(dta2, year==2003&week %in% c(19:39))
    #dta_yr <- subset(dta2, year_re==2003&month %in% c(5:9))
    #data.pred_yr <- subset(data.pred2, week!=53)
    data.pred_yr <- subset(dta.pred.deaths2, year==2003&week %in% c(19:39))
    #data.pred_yr <- subset(data.pred2, year==2003&month %in% c(5:9))
    hwDAYS <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0)
  } else {
    if (yr == "2015") {
      #dta_yr <- subset(dta2, week!=53)
      dta_yr <- subset(dta2, year==2015&week %in% c(19:39))
      #dta_yr <- subset(dta2, year==2015&month %in% c(5:9))
      #data.pred_yr <- subset(data.pred2, week!=53)
      data.pred_yr <- subset(dta.pred.deaths2, year==2015&week %in% c(19:39))
      #data.pred_yr <- subset(data.pred2, year==2015&month %in% c(5:9))
      hwDAYS <- c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0)
    } else {
      if (yr == "2018") {
        #dta_yr <- subset(dta2, week!=53)
        dta_yr <- subset(dta2, year==2018&week %in% c(19:39))
        #dta_yr <- subset(dta2, year==2018&month %in% c(5:9))
        #data.pred_yr <- subset(data.pred2, week!=53)
        data.pred_yr <- subset(dta.pred.deaths2, year==2018&week %in% c(19:39))
        #data.pred_yr <- subset(data.pred2, year==2018&month %in% c(5:9))
        hwDAYS <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0)
      } else {
        print("No year was selected!")
      }
    }
  }
}

# Calculate percentage of excess-mortality
dta_yr$excess <- ((dta_yr$deaths - data.pred_yr$deaths) / data.pred_yr$deaths)*100
dta_yr$posexcess <- dta_yr$excess; dta_yr$posexcess[dta_yr$posexcess<0] <-0
dta_yr$negexcess <- dta_yr$excess; dta_yr$negexcess[dta_yr$negexcess>0] <-0

# DEFINITION EXCESS-MORTALITY: Vicedo SMW 2016:
## "The percentage excess mortality corresponds to the number
## of excess deaths divided by the number of expected deaths."

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
####################
# Evaluate Results #
####################

# hot event deaths per day:
dta_yr$hot <- hwDAYS

# calculate daily mortality for a hot day:
RES_RATE[paste0(citta),paste0(yr,": Hot Week", sep="")] <- sum(dta_yr$deaths * dta_yr$hot) / sum(dta_yr$hot)
RES_PERC[paste0(citta),paste0(yr,": Hot Week", sep="")] <- sum(dta_yr$excess * dta_yr$hot) / sum(dta_yr$hot)

# calculate average daily mortality for a summer day:
RES_RATE[paste0(citta),paste0(yr,": Average Week", sep="")] <- sum(dta_yr$deaths) / length(dta_yr$week) # rate
RES_PERC[paste0(citta),paste0(yr,": Average Week", sep="")] <- sum(dta_yr$excess) / length(dta_yr$week) # percentage

####################
####################

# PLOT Excess Mortality for the selected summer:
dummy_factor <- max(dta_yr$posexcess)+15
summer <- c(19,22,26,31,39) # weeks 19 -39 (Start May - end September)
#summer_m <- c(19,22,26,31,35) # months
summer_m <- c(19:39) # months
summer_t <- c("W19 May","W20 May", "W21 May",
              "W22 Jun","W23 Jun","W24 Jun","W25 Jun",
              "W26 Jul","W27 Jul","W28 Jul","W29 Jul","W30 Jul",
              "W31 Aug","W32 Aug","W33 Aug","W34 Aug",
              "W35 Sep","W36 Sep","W37 Sep","W38 Sep","W39 Sep")
#summer_minor <- dta_yr$week

# Heatwaves

# 1947: 22.07.-04.08. und 12.08.-21.08. -> KW 30, 31, 32, 33, 34
# 2003: 01.08.-13.08. -> KW 31, 32, 33
# 2015: 01.07.-07.07. und 16.-24.07. -> KW 27, 28, 29, 30
# 2018: 30.07.-08.08. -> KW 31, 32
hwDAYS_1947 <- c(0,0,0,
                 0,0,0,0,
                 0,0,0,0,1,
                 1,1,1,1,
                 0,0,0,0,0)

hwDAYS_2003 <- c(0,0,0,
                 0,0,0,0,
                 0,0,0,0,0,
                 1,1,1,0,
                 0,0,0,0,0)

hwDAYS_2015 <- c(0,0,0,
                 0,0,0,0,
                 0,1,1,1,1,
                 0,0,0,0,
                 0,0,0,0,0)

hwDAYS_2018 <- c(0,0,0,
                 0,0,0,0,
                 0,0,0,0,0,
                 1,1,0,0,
                 0,0,0,0,0)


ex_mort_yr <- ggplot(data=dta_yr) +
  #geom_segment( aes(x=date, xend=date, y=-0*d_compound_lag, yend=dummy_factor*d_compound_lag), color=col_reg[8], size=2.0, alpha=0.001) +
  geom_segment( aes(x=week, xend=week, y=dummy_factor*hwDAYS*0, yend= dummy_factor*hwDAYS), color=col_reg[7], size=9, alpha=0.3) +
  #geom_segment( aes(x=date, xend=date, y=-d_humid_lag*0, yend=(dummy_factor - 10)*d_humid_lag), color=col_reg[1], size=2.0, alpha=0.3) +
  geom_segment( aes(x=week, xend=week, y=0, yend=posexcess), color=col_reg[6], size=5) +
  geom_segment( aes(x=week, xend=week, y=0, yend=negexcess), color=col_reg[5], size=5) +
  #geom_line( aes(x=date, y=`max temperature [deg]`+50),  color =col_reg[8],  size=.5) +
  #geom_line( aes(x=date, y=`partial pressure [hPa]`+50), color =col_reg[2],  size=.5) +
  theme_minimal() +
  ggtitle(paste0("All-cause weekly mortality: Summer ", yr, "\n", citta, sep=" ")) +
  scale_x_continuous(name= "weeks", breaks=summer_m, minor_breaks=summer_m, labels=summer_t, limits=c(min(summer), max(summer))) +
  guides(x =  guide_axis(angle = 60)) +
  scale_y_continuous(name = "percentage of excess mortality")
  #                 sec.axis = sec_axis(~.-50, name = "maximum temperature (\u00B0C) and humidity as mean partial pressure (hPa)", breaks=c(0,10,20,30,40)))
ex_mort_yr

# prepare a legend seperately:
legend_ex_mort_yr <- ggplot(data=dta_yr) +
  #geom_segment( aes(x=date, xend=date, y=-0*d_compound_lag*dummy_factor, yend=dummy*dummy_factor, color="Compound Hot-Humid Day"), size=1.35, alpha=0.1) +
  geom_segment( aes(x=week, xend=week, y=dummy_factor*hwDAYS*0, yend=dummy_factor*hwDAYS, color="Heatwave Exposure"),size=1.35, alpha=0.3) +
  #geom_segment( aes(x=date, xend=date, y=-d_humid_lag*0, yend=limit_humi*d_humid_lag, color="Humid Day"),size=1.35, alpha=0.3) +
  geom_segment( aes(x=week, xend=week, y=0, yend=posexcess, color="Positive Excess Mortality"), size=1.35) +
  geom_segment( aes(x=week, xend=week, y=0, yend=negexcess, color="Negative Excess Mortality"), size=1.35) +
  #geom_line( aes(x=date, y=`max temperature [deg]`, color="Daily Maxmimum Temperature"), size=.9) +
  #geom_line( aes(x=date, y=`partial pressure [hPa]`, color="Daily Mean Partial Pressure"),  size=.9) +
  scale_color_manual(values = c(col_reg[7], col_reg[6], col_reg[5]),
                    labels = c("Heatwave Exposure", "Positive Excess Mortality", "Negative Excess Mortality")) +
  scale_linetype_manual(values = c('solid','solid', 'solid')) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  theme(legend.position="right")
#legend_ex_mort_yr


# extract the legend
legend_only <- as_ggplot(get_legend(legend_ex_mort_yr))
#legend_only

# merge the plot w the legend via cowplot
#ex_mort_yr <- plot_grid(ex_mort_yr, NULL, legend_only, NULL, nrow= 1, rel_widths= c(1, 0.18, 0.2, 0.18))
ex_mort_yr <- plot_grid(ex_mort_yr, NULL, legend_only, NULL, nrow= 1, rel_widths= c(1, 0.001, 0.2, 0.01))
ex_mort_yr
#test <- plot_grid(e_m_summer_yr, legend_only, ncol=1, rel_heights = c(1,0.2))

ggsave(paste0(getwd(), "/img/", citta, "_", yr, "_ex_mort", ".pdf", sep=""), device = "pdf", width=13)



} # yr loop
# 
# #dev.off()
# 
# } # cant loop

} # i loop
# 
write.xlsx(RES_RATE, file = paste0(getwd(), "/output/RES_RATE.xlsx", sep=""), sheetName = "death_rates",
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(RES_PERC, file = paste0(getwd(), "/output/RES_RATE.xlsx", sep=""), sheetName = "death_percentages",
           col.names = TRUE, row.names = TRUE, append = TRUE)
# 
# return("Done")
}

##################
# CONCATENATE PDF#
##################

# update working directory
setwd("D:/data/mortalityCHcities/img")

# Mortality 
pdf_combine(c("Basel_mort_pred_obs-41-60.pdf", "Bern_mort_pred_obs-41-60.pdf", "Biel_mort_pred_obs-41-60.pdf", "Geneva_mort_pred_obs-41-60.pdf", "La Chaux-de-Fonds_mort_pred_obs-41-60.pdf", "Lausanne_mort_pred_obs-41-60.pdf", "Lucerne_mort_pred_obs-41-60.pdf", "St. Gallen_mort_pred_obs-41-60.pdf", "Winterthur_mort_pred_obs-41-60.pdf", "Zurich_mort_pred_obs-41-60.pdf"), "summary/mort_pred_obs-41-60.pdf")

pdf_combine(c("Basel_mort_pred_obs-99-18.pdf", "Bern_mort_pred_obs-99-18.pdf", "Biel_mort_pred_obs-99-18.pdf", "Geneva_mort_pred_obs-99-18.pdf", "La Chaux-de-Fonds_mort_pred_obs-99-18.pdf", "Lausanne_mort_pred_obs-99-18.pdf", "Lucerne_mort_pred_obs-99-18.pdf", "St. Gallen_mort_pred_obs-99-18.pdf", "Winterthur_mort_pred_obs-99-18.pdf", "Zurich_mort_pred_obs-99-18.pdf"), "summary/mort_pred_obs-99-18.pdf")

pdf_combine(c("Basel_1947_ex_mort.pdf", "Basel_2003_ex_mort.pdf", "Basel_2015_ex_mort.pdf", "Basel_2018_ex_mort.pdf"), "summary/Basel_ex_mort.pdf")
pdf_combine(c("Bern_1947_ex_mort.pdf", "Bern_2003_ex_mort.pdf", "Bern_2015_ex_mort.pdf", "Bern_2018_ex_mort.pdf"), "summary/Bern_ex_mort.pdf")
pdf_combine(c("Biel_1947_ex_mort.pdf", "Biel_2003_ex_mort.pdf", "Biel_2015_ex_mort.pdf", "Biel_2018_ex_mort.pdf"), "summary/Biel_ex_mort.pdf")
pdf_combine(c("Geneva_1947_ex_mort.pdf", "Geneva_2003_ex_mort.pdf", "Geneva_2015_ex_mort.pdf", "Geneva_2018_ex_mort.pdf"), "summary/Geneva_ex_mort.pdf")
pdf_combine(c("La Chaux-de-Fonds_1947_ex_mort.pdf", "La Chaux-de-Fonds_2003_ex_mort.pdf", "La Chaux-de-Fonds_2015_ex_mort.pdf", "La Chaux-de-Fonds_2018_ex_mort.pdf"), "summary/La Chaux-de-Fonds_ex_mort.pdf")
pdf_combine(c("Lausanne_1947_ex_mort.pdf", "Lausanne_2003_ex_mort.pdf", "Lausanne_2015_ex_mort.pdf", "Lausanne_2018_ex_mort.pdf"), "summary/Lausanne_ex_mort.pdf")
pdf_combine(c("Lucerne_1947_ex_mort.pdf", "Lucerne_2003_ex_mort.pdf", "Lucerne_2015_ex_mort.pdf", "Lucerne_2018_ex_mort.pdf"), "summary/Lucerne_ex_mort.pdf")
pdf_combine(c("St. Gallen_1947_ex_mort.pdf", "St. Gallen_2003_ex_mort.pdf", "St. Gallen_2015_ex_mort.pdf", "St. Gallen_2018_ex_mort.pdf"), "summary/St. Gallen_ex_mort.pdf")
pdf_combine(c("Winterthur_1947_ex_mort.pdf", "Winterthur_2003_ex_mort.pdf", "Winterthur_2015_ex_mort.pdf", "Winterthur_2018_ex_mort.pdf"), "summary/Winterthur_ex_mort.pdf")
pdf_combine(c("Zurich_1947_ex_mort.pdf", "Zurich_2003_ex_mort.pdf", "Zurich_2015_ex_mort.pdf", "Zurich_2018_ex_mort.pdf"), "summary/Zurich_ex_mort.pdf")

              
#######
# END #
#######