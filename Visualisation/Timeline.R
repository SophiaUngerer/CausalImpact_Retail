###########################################################################################################################
######################################################## Timeline ################## ######################################
###########################################################################################################################

#################################################### Initiation ###########################################################
##### Packages #####
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gt)

##### Data #####
setwd("~/OneDrive - Universität St.Gallen/Thesis/Code")
covid_cases <- read_csv("COVID19Cases_geoRegion.csv")

covid_cases <- covid_cases %>%
  subset(covid_cases$geoRegion == "CH")%>%
  subset(datum < "2022-01-01")

covid_cases <- covid_cases[,2:4]


##### Colorpalette #####
custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")
################################################### Visualisation ##########################################################
##### Timeline #####
lines <- data.frame(vlines = as.Date(c("2020-03-01", "2020-05-01", "2020-06-15", "2021-01-01", "2021-03-01")), 
                    labels = c("Start Lockdown & Borders close","End Lockdown", "Borders open", "Start Lockdown", "End Lockdown"), stringsAsFactors = FALSE)

timeline <- ggplot(covid_cases, aes(x=datum, y=`entries`))+
  geom_rect(aes(xmin = as.Date("2020-03-01"),xmax = as.Date("2020-05-01"), ymin = 00, ymax = 20000),
            color = custom.col[5], fill=custom.col[5])+
  geom_rect(aes(xmin = as.Date("2021-01-01"),xmax = as.Date("2021-03-01"),ymin = 00, ymax = 20000),
            color = custom.col[5], fill=custom.col[5])+
  geom_area( color="grey10",
             fill="grey10", alpha = 0.5)+
  theme_classic()+
  scale_x_date(date_labels = "%B %Y", limits = c(as.Date("2020-01-01"), as.Date("2021-12-01")))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  geom_vline(data = lines, aes(xintercept = vlines), size=0.5, show.legend = TRUE, linetype="dashed") +
  geom_text(data = lines, aes(x = vlines -15, y = 10000, label = labels, angle = 90), 
            size=5)+
  labs(y = "Covid-19 Cases", 
       x = "Date",
       title = "Timeline Covid-19 Meaures Switzerland")+
  theme(text=element_text(size=20), 
        axis.text=element_text(size=15), 
        axis.title=element_text(size=15), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=15), 
        legend.title=element_text(size=15))

####################################################### Print ##############################################################
##### Save Data #####
ggsave(plot = timeline, width = 13.5, height = 6, dpi = 300, filename = "timeline.png")

