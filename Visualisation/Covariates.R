###########################################################################################################################
######################################################## Covariates ########################################################
###########################################################################################################################

#################################################### Initiation ###########################################################
##### Packages #####
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gt)
library(lubridate)
library(tidyr)


##### Data #####
setwd("~/OneDrive - Universität St.Gallen/Thesis/Code")
covid_cases <- read_csv("COVID19Cases_geoRegion.csv")

covid_cases <- covid_cases %>%
  subset(covid_cases$geoRegion == "CH")%>%
  subset(datum < "2022-01-01")

covid_cases <- covid_cases[,2:4]


# Download variables from the High Frequency Tracking Board
ch_variables <- read.csv(url("https://raw.githubusercontent.com/KOF-ch/economic-monitoring/master/data/ch.kof.stringency.csv"))

# Filter for data relevant to Switzerland
ch_variables <- 
  ch_variables %>% 
  filter(geo == "ch")

# Select monthly data for border closings 
travel_int <- filter(ch_variables, variable == "c8_internationaltravel")
travel_int$time <- as.Date(travel_int$time)
travel_int$month <- floor_date(travel_int$time, "month")
travel_int <- travel_int[!duplicated(travel_int$month), ]
travel_int <- filter(travel_int, time < "2022-01-01")
travel_int$value <- (travel_int$value)*3


# Select monthly data for workplace closings 
workplace <- filter(ch_variables, variable == "c2_workplaceclosing")
workplace$time <- as.Date(workplace$time)
workplace$month <- floor_date(workplace$time, "month")
workplace <- workplace[!duplicated(workplace$month), ]
workplace <- filter(workplace, time < "2022-01-01")
workplace$value <- (workplace$value)*4


# Create a covariate for restaurant closings
restaurant <- c(0,0,2,2,1,0,0,0,0,0,0,2,2,2,2,1,1,0,0,0,0,0,0,0)
restaurant <- restaurant*6

covariates <- data.frame(travel_int$time, travel_int$value, workplace$value, restaurant)
colnames(covariates) <- c("date", "Border Closings", 
                          "Home Office Requirement", "Restaurant Closings")

covariates1 <- gather(covariates,"Variable", "Numbers", 2:4)

##### Colorpalette ##### 
custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")
################################################### Visualisation ##########################################################
##### Timeline #####

cov_plot <- ggplot(covariates1, aes(x=`date`, y=`Numbers`, group=Variable, 
                                    color=Variable))+
  geom_line()+
  theme_classic()+
  scale_x_date(date_labels = "%B %Y", limits = c(as.Date("2020-01-01"), as.Date("2021-12-01")))+
  labs(y = "Values", 
       x = "Date",
       title = "Covariates") +
  scale_color_manual(values=c(custom.col[2], custom.col[4],custom.col[6]))+
  scale_y_continuous(breaks = NULL) +
  theme(text=element_text(size=20), 
        axis.text=element_text(size=15), 
        axis.title=element_text(size=15), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        legend.title=element_text(size=15))



cov_plot
####################################################### Print ##############################################################
##### Save Data #####
ggsave(plot = cov_plot, width = 13.5, height = 6, dpi = 300, filename = "covariates.png")

