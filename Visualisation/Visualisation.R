###########################################################################################################################
###################################################### Visualisation ######################################################
###########################################################################################################################

#################################################### Initiation ###########################################################
##### Packages #####
library(tseries)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(zoo)
library(gt)

##### Import Data #####
setwd("~/OneDrive - Universität St.Gallen/Thesis/Code")   # Select a working directory, where the data is stored
revenue_import <- read.csv("11_Detailhandelsstatistik_220211.csv", skip=2) # Retrieved from XXX

################################################### Data Cleaning ##########################################################
##### Data Selection #####
revenue_brut <- data.frame(revenue_import[c(26:29),-c(1:8)]) # The raw revenue data is used for the analysis
revenue_brut <- as.data.frame(t(revenue_brut))

### Define dates ###
revenue_brut$date <- seq(as.Date("2000-1-1"), as.Date("2021-12-1"), by = "month")
rownames(revenue_brut) <- NULL

##### Variable Names #####
revenue_brut <- revenue_brut[, c(5, 1, 2, 3, 4)]
colnames(revenue_brut) <- c("Date" , 
                            "Total_Retail", 
                            "Food_Drinks_Tobacco", 
                            "Clothes_Shoes", 
                            "Others")

### Ensure the values are numeric ###
revenue_brut[,2] <- as.numeric(revenue_brut[,2])
revenue_brut[,3] <- as.numeric(revenue_brut[,3])
revenue_brut[,4] <- as.numeric(revenue_brut[,4])
revenue_brut[,5] <- as.numeric(revenue_brut[,5])
revenue_brut[,4] <- revenue_brut[,4] + revenue_brut[,5]
revenue_brut[,4] <- revenue_brut[,4]/2 #Make non-essentails the average from clothes and other retail

##### Colorpalette #####
custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


################################################### Visualisation ##########################################################
##### Data Selection #####
total <- c("Date", "Total_Retail")
food <- c("Date", "Food_Drinks_Tobacco")
clothes <- c("Date", "Clothes_Shoes")

revenue_brut_total <- revenue_brut[total]
revenue_brut_food <- revenue_brut[food]
revenue_brut_clothes <- revenue_brut[clothes]

revenue_brut_total <- revenue_brut_total[1:262,]
revenue_brut_food <- revenue_brut_food[1:262,]
revenue_brut_clothes <- revenue_brut_clothes[1:262,]

##### Total Retail #####
p_total <- ggplot(revenue_brut_total, aes(x=Date, y=`Total_Retail`))+
  labs(y = "Turnover", 
       title = "Development of Total Retail Turnover")+
  geom_rect(aes(xmin = as.Date("2020-03-01"),xmax = as.Date("2020-05-01"),ymin = 65, ymax = Inf),
            fill= custom.col[5], 
            alpha = 0.04)+
  geom_rect(aes(xmin = as.Date("2021-01-01"),xmax = as.Date("2021-03-01"),ymin = 65, ymax = Inf),
            fill= custom.col[5], 
            alpha = 0.04)+
  geom_line(color = "black")+
  theme_classic()+
  geom_smooth(aes(x=Date, y=`Total_Retail`), colour = custom.col[6], method = loess, se = FALSE, size = 0.5)+
  theme(text=element_text(size=10), 
        axis.text=element_text(size=10), 
        axis.title=element_text(size=15), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10))

##### Essential Retail #####
p_food <- ggplot(revenue_brut_food, aes(x=Date, y=`Food_Drinks_Tobacco`))+
  labs(y = "Turnover", 
       title = "Development of Essential Retail")+
  geom_rect(aes(xmin = as.Date("2020-03-01"),xmax = as.Date("2020-05-01"),ymin = 70, ymax = Inf),
            fill= custom.col[5], 
            alpha = 0.04)+
  geom_rect(aes(xmin = as.Date("2021-01-01"),xmax = as.Date("2021-03-01"),ymin = 70, ymax = Inf),
            fill= custom.col[5], 
            alpha = 0.04)+
  geom_line(color = "black")+
  theme_classic()+
  geom_smooth(aes(x=Date, y=`Food_Drinks_Tobacco`), colour = custom.col[6], method = loess, se = FALSE, size = 0.5)+
  theme(text=element_text(size=10), 
        axis.text=element_text(size=10), 
        axis.title=element_text(size=15), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10))

##### Non-Essential Retail #####
p_clothes <- ggplot(revenue_brut_clothes, aes(x=Date, y=`Clothes_Shoes`))+
  labs(y = "Turnover", 
       title = "Development of Non-Essential Retail Turnover")+
  geom_rect(aes(xmin = as.Date("2020-03-01"),xmax = as.Date("2020-05-01"),ymin = 30, ymax = Inf),
            fill= custom.col[5], 
            alpha = 0.04)+
  geom_rect(aes(xmin = as.Date("2021-01-01"),xmax = as.Date("2021-03-01"),ymin = 30, ymax = Inf),
            fill= custom.col[5], 
            alpha = 0.04)+
  geom_line(color = "black")+
  theme_classic()+
  geom_smooth(aes(x=Date, y=`Clothes_Shoes`), colour = custom.col[6], method = loess, se = FALSE, size = 0.5)+
  theme(text=element_text(size=10), 
        axis.text=element_text(size=10), 
        axis.title=element_text(size=15), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10))

################################################### Comparison ##########################################################
##### Comparison Total ####
### Create Lists ###
total_2019 <- revenue_brut$Total_Retail[229:240]
total_2020 <- revenue_brut$Total_Retail[241:252]
total_2O21 <- revenue_brut$Total_Retail[253:264]
month <- c("January", "February", "March", "April", "May", "June", 
           "July", "August", "September", "October", "November", "December")
### Create Table ### 
total_impact <- data.frame(month, total_2019, total_2020, total_2O21)
colnames(total_impact) <- c("Month", "2019", "2020", "2021")

### Calculate Impact ### 
for (i in 1:12) {
  x <- (round((100/total_impact$`2019`[i]*total_impact$`2020`[i]), 2) - 100)
  total_impact$Impact1[i] <- x
}  
for (i in 1:12) {
  x <- (round((100/total_impact$`2019`[i]*total_impact$`2021`[i]), 2) - 100)
  total_impact$Impact2[i] <- x
}  

colnames(total_impact) <- c("Month", "2019", "2020", "2021" , "2019 to 2020", "2019 to 2021")

### Output ###
total_impact %>%
  gt(rowname_col =  "Month") %>%
  tab_spanner(label = "Absolute Turnover",
              columns = c("2019", "2020", "2021")) %>%
  tab_spanner(label = "Difference (%)", 
              c("2019 to 2020", "2019 to 2021"))%>%
  tab_header(
    title = "Total Retail Turnover",
    subtitle = "Comparing the years 2020 and 2021 with the year 2019"
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#F4EDCA"),
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = c("2019 to 2020"), 
      rows = 3:4)
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#F4EDCA"),
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = c("2019 to 2021"), 
      rows = 1:2)
  )%>%
  tab_source_note(html('<pre><span style="background-color: #F4EDCA"
                       >        </span> = Lockdown</pre>'))

##### Comparison Food ####
### Create Lists ###
food_2019 <- revenue_brut$Food_Drinks_Tobacco[229:240]
food_2020 <- revenue_brut$Food_Drinks_Tobacco[241:252]
food_2O21 <- revenue_brut$Food_Drinks_Tobacco[253:264]

### Create Table ### 
food_impact <- data.frame(month, food_2019, food_2020, food_2O21)
colnames(food_impact) <- c("Month", "2019", "2020", "2021")

### Calculate Impact ### 
for (i in 1:12) {
  x <- (round((100/food_impact$`2019`[i]*food_impact$`2020`[i]), 2) - 100)
  food_impact$Impact1[i] <- x
}  
for (i in 1:12) {
  x <- (round((100/food_impact$`2019`[i]*food_impact$`2021`[i]), 2) - 100)
  food_impact$Impact2[i] <- x
}  

colnames(food_impact) <- c("Month", "2019", "2020", "2021" , "2019 to 2020", "2019 to 2021")

### Output ###
food_impact %>%
  gt(rowname_col =  "Month") %>%
  tab_spanner(label = "Absolute Turnover",
              columns = c("2019", "2020", "2021")) %>%
  tab_spanner(label = "Difference (%)", 
              c("2019 to 2020", "2019 to 2021"))%>%
  tab_header(
    title = "Essential Retail Turnover",
    subtitle = "Comparing the years 2020 and 2021 with the year 2019"
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#F4EDCA"),
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = c("2019 to 2020"), 
      rows = 3:4)
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#F4EDCA"),
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = c("2019 to 2021"), 
      rows = 1:2)
  )%>%
  tab_source_note(html('<pre><span style="background-color: #F4EDCA"
                       >        </span> = Lockdown</pre>'))
##### Comparison Clothes ####
### Create Lists ###
clothes_2019 <- revenue_brut$Clothes_Shoes[229:240]
clothes_2020 <- revenue_brut$Clothes_Shoes[241:252]
clothes_2O21 <- revenue_brut$Clothes_Shoes[253:264]

### Create Table ### 
clothes_impact <- data.frame(month, clothes_2019, clothes_2020, clothes_2O21)
colnames(clothes_impact) <- c("Month", "2019", "2020", "2021")

### Calculate Impact ### 
for (i in 1:12) {
  x <- (round((100/clothes_impact$`2019`[i]*clothes_impact$`2020`[i]), 2) - 100)
  clothes_impact$Impact1[i] <- x
}  
for (i in 1:12) {
  x <- (round((100/clothes_impact$`2019`[i]*clothes_impact$`2021`[i]), 2) - 100)
  clothes_impact$Impact2[i] <- x
}  

colnames(clothes_impact) <- c("Month", "2019", "2020", "2021" , "2019 to 2020", "2019 to 2021")

### Output ###
clothes_impact %>%
  gt(rowname_col =  "Month") %>%
  tab_spanner(label = "Absolute Turnover",
              columns = c("2019", "2020", "2021")) %>%
  tab_spanner(label = "Difference (%)", 
              c("2019 to 2020", "2019 to 2021"))%>%
  tab_header(
    title = "Non-Essential Retail Turnover",
    subtitle = "Comparing the years 2020 and 2021 with the year 2019"
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#F4EDCA"),
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = c("2019 to 2020"), 
      rows = 3:4)
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#F4EDCA"),
      cell_text(weight = 'bold')
    ),
    locations = cells_body(
      columns = c("2019 to 2021"), 
      rows = 1:2)
  )%>%
  tab_source_note(html('<pre><span style="background-color: #F4EDCA"
                       >        </span> = Lockdown </pre>'))

####################################################### Print ##############################################################
##### Save Data #####
ggsave(plot = p_total, width = 13.5, height = 6, dpi = 300, filename = "data_total.png")
ggsave(plot = p_food, width = 13.5, height = 6, dpi = 300, filename = "data_essential.png")
ggsave(plot = p_clothes, width = 13.5, height = 6, dpi = 300, filename = "data_non_essential.png")

