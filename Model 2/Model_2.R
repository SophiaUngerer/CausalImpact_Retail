###########################################################################################################################
######################################################### Model 2 #########################################################
###########################################################################################################################

#################################################### Initiation ###########################################################
##### Packages #####
library(tidyverse)
library(ggplot2)
library(dplyr)
library(zoo)
library(CausalImpact)
library(bsts)
library(gt)
library(stats)
library(lubridate)

##### Import Data #####
setwd("~/OneDrive - Universität St.Gallen/Thesis/Code")   # Select a working directory, where the data is stored
revenue_import <- read.csv("11_Detailhandelsstatistik_220211.csv", skip=2) # Retrieved from https://www.bfs.admin.ch/bfs/de/home/aktuell/covid-19.assetdetail.21364465.html

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

##### Import Covariates #####
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

# Select monthly data for workplace closings 
workplace <- filter(ch_variables, variable == "c2_workplaceclosing")
workplace$time <- as.Date(workplace$time)
workplace$month <- floor_date(workplace$time, "month")
workplace <- workplace[!duplicated(workplace$month), ]
workplace <- filter(workplace, time < "2022-01-01")

# Create a covariate for restaurant closings
restaurant <- c(0,0,2,2,1,0,0,0,0,0,0,2,2,2,2,1,1,0,0,0,0,0,0,0)


##### Add Coviariates #####
revenue_brut_covar <- revenue_brut
revenue_brut_covar$travel_int <- 0
revenue_brut_covar$travel_int[241:264] <- travel_int$value

revenue_brut_covar$workplace <- 0
revenue_brut_covar$workplace[241:264] <- workplace$value

revenue_brut_covar$restaurants <- 0
revenue_brut_covar$restaurants[241:264] <- restaurant

################################################ Variable Definition #######################################################
##### General Variables #####
### Define a pre- and a post-period
pre.period <- c(1, 240)   
post.period1 <- c(243,252) # Post-period for the first Lockdown
post.period2 <- c(253,262) # Post-period for the second Lockdown

### Select Regression Variables Lockdown 1
x1_1 <-(revenue_brut_covar$travel_int [1:252]) # x1 = international border restrictions, it being an indicator of shopping tourism
x2_1 <-(revenue_brut_covar$workplace[1:252])  # x2 = Workplace closing
x3_1 <-(revenue_brut_covar$restaurants[1:252]) # x3 = Restaurant closings

### Select Regression Variables Lockdown 2
x1_2 <-(revenue_brut_covar$travel_int[1:262]) # x1 = international border restrictions, it being an indicator of shopping tourism
x2_2 <-(revenue_brut_covar$workplace[1:262])  # x2 = Workplace closing
x3_2 <-(revenue_brut_covar$restaurants[1:262]) # x3 = Restaurant closings

################################################# Model Lockdown 1 ########################################################
##### Model 2.1 - Total Retail #####
# Select variables for the regression
y_total_1<-(revenue_brut_covar$Total_Retail[1:252])  

# Assign the pre- and a post-period
post.period.response1 <- y_total_1[post.period1[1] : post.period1[2]]

# Remove the outcome from the post-period 
y_total_1[post.period1[1] : post.period1[2]] <- NA

# Create a zoo object
time.points <-seq(as.Date("2000-1-1"),by="month", length.out = 252)
model.data1_total <- zoo(cbind(y_total_1, x1_1, x2_1, x3_1), time.points)

### Model ###
# Add a local and a seasonal trend
sdy <- sd(y_total_1, na.rm = TRUE)
ss<- NA
ss <- list()
ss <- AddLocalLevel(ss, y_total_1)
ss<-AddSeasonal(ss,y_total_1, nseasons = 12)

# BTST model
reg.model1_total <- bsts(y_total_1 ~ ., 
                         state.specification = ss, 
                         data = model.data1_total, 
                         niter = 100000) # Number of runs

### Prediction ###
reg_impact1_total<-CausalImpact(bsts.model = reg.model1_total,
                                post.period.response = post.period.response1)

### Results ###
summary(reg_impact1_total$model$bsts.model)
summary(reg_impact1_total) 
plot(reg_impact1_total) + theme_minimal(base_size = 10) + labs(title = "Total Lockdown 2, Regression Model", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


##### Model 2.2 - Essential Retail #####
# Select variables for the regression
y_food_1<-(revenue_brut_covar$Food_Drinks_Tobacco[1:252])  

# Assign the pre- and a post-period
post.period.response1 <- y_food_1[post.period1[1] : post.period1[2]]

# Remove the outcome from the post-period 
y_food_1[post.period1[1] : post.period1[2]] <- NA

# Create a zoo object
time.points <-seq(as.Date("2000-1-1"),by="month", length.out = 252)
model.data1_food <- zoo(cbind(y_food_1, x1_1, x2_1, x3_1), time.points)

### Model ###
# Add a local and a seasonal trend
sdy <- sd(y_food_1, na.rm = TRUE)
ss<- NA
ss <- list()
ss <- AddLocalLevel(ss, y_food_1)
ss<-AddSeasonal(ss,y_food_1, nseasons = 12)

# BTST model
reg.model1_food <- bsts(y_food_1 ~ ., 
                        state.specification = ss, 
                        data = model.data1_food, 
                        niter = 100000) # Number of runs

### Prediction ###
reg_impact1_food<-CausalImpact(bsts.model = reg.model1_food,
                                post.period.response = post.period.response1)

### Results ###
summary(reg_impact1_food$model$bsts.model)
summary(reg_impact1_food) 
plot(reg_impact1_food) + theme_minimal(base_size = 10) + labs(title = "Essentials Lockdown 1, Regression Model", size = 10)+
  theme(text = element_text(family = "Times New Roman"))

##### Model 2.3 - Non-Essential Retail #####
# Select variables for the regression
y_clothes_1<-(revenue_brut_covar$Clothes_Shoes[1:252])  

# Assign the pre- and a post-period
post.period.response1 <- y_clothes_1[post.period1[1] : post.period1[2]]

# Remove the outcome from the post-period 
y_clothes_1[post.period1[1] : post.period1[2]] <- NA

# Create a zoo object
time.points <-seq(as.Date("2000-1-1"),by="month", length.out = 252)
model.data1_clothes <- zoo(cbind(y_clothes_1,x1_1, x2_1, x3_1), time.points)

### Model ###
# Add a local and a seasonal trend
sdy <- sd(y_clothes_1, na.rm = TRUE)
ss<- NA
ss <- list()
ss <- AddLocalLevel(ss, y_clothes_1)
ss<-AddSeasonal(ss,y_clothes_1, nseasons = 12)

# BTST model
reg.model1_clothes <- bsts(y_clothes_1 ~ ., 
                         state.specification = ss, 
                         data = model.data1_clothes, 
                         niter = 100000) # Number of runs

### Prediction ###
reg_impact1_clothes <-CausalImpact(bsts.model = reg.model1_clothes,
                                post.period.response = post.period.response1)

### Results ###
summary(reg_impact1_clothes$model$bsts.model)
summary(reg_impact1_clothes) 
plot(reg_impact1_clothes) + theme_minimal(base_size = 10) + labs(title = "Non-Essentials Lockdown 1, Regression Model", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


################################################# Model Lockdown 2 ########################################################
##### Model 2.4 - Total Retail #####
# Select variables for the regression
y_total_2<-(revenue_brut_covar$Total_Retail[1:262])  

# Assign the pre- and a post-period
post.period.response2 <- y_total_2[post.period2[1] : post.period2[2]]

# Remove the outcome from the post-period 
y_total_2[post.period2[1] : post.period2[2]] <- NA

# Create a zoo object
time.points <-seq(as.Date("2000-1-1"),by="month", length.out = 262)
model.data2_total <- zoo(cbind(y_total_2, x1_2, x2_2, x3_2), time.points)

### Model ###
# Add a local and a seasonal trend
sdy <- sd(y_total_2, na.rm = TRUE)
ss<- NA
ss <- list()
ss <- AddLocalLevel(ss, y_total_2)
ss<-AddSeasonal(ss,y_total_2, nseasons = 12)

# BTST model
reg.model2_total <- bsts(y_total_2 ~ ., 
                         state.specification = ss, 
                         data = model.data2_total, 
                         niter = 100000) # Number of runs

### Prediction ###
reg_impact2_total<-CausalImpact(bsts.model = reg.model2_total,
                                post.period.response = post.period.response2)

### Results ###
summary(reg_impact2_total$model$bsts.model)
summary(reg_impact2_total) 
plot(reg_impact2_total) + theme_minimal(base_size = 10) + labs(title = "Total Lockdown 2, Regression Model", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


##### Model 2.5 - Essential Retail #####
# Select variables for the regression
y_food_2<-(revenue_brut_covar$Food_Drinks_Tobacco[1:262])  

# Assign the pre- and a post-period
post.period.response2 <- y_food_2[post.period2[1] : post.period2[2]]

# Remove the outcome from the post-period 
y_food_2[post.period2[1] : post.period2[2]] <- NA

# Create a zoo object
time.points <-seq(as.Date("2000-1-1"),by="month", length.out = 262)
model.data2_food <- zoo(cbind(y_food_2, x1_2, x2_2, x3_2), time.points)

### Model ###
# Add a local and a seasonal trend
sdy <- sd(y_food_2, na.rm = TRUE)
ss<- NA
ss <- list()
ss <- AddLocalLevel(ss, y_food_2)
ss<-AddSeasonal(ss,y_food_2, nseasons = 12)

# BTST model
reg.model2_food <- bsts(y_food_2 ~ ., 
                        state.specification = ss, 
                        data = model.data2_food, 
                        niter = 100000) # Number of runs

### Prediction ###
reg_impact2_food<-CausalImpact(bsts.model = reg.model2_food,
                               post.period.response = post.period.response2)

### Results ###
summary(reg_impact2_food$model$bsts.model)
summary(reg_impact2_food) 
plot(reg_impact2_food) + theme_minimal(base_size = 10) + labs(title = "Essentials Lockdown 2, Regression Model", size = 10)+
  theme(text = element_text(family = "Times New Roman"))

##### Model 2.6 - Non-Essential Retail #####
# Select variables for the regression
y_clothes_2<-(revenue_brut_covar$Clothes_Shoes[1:262])  

# Assign the pre- and a post-period
post.period.response2 <- y_clothes_2[post.period2[1] : post.period2[2]]

# Remove the outcome from the post-period 
y_clothes_2[post.period2[1] : post.period2[2]] <- NA

# Create a zoo object
time.points <-seq(as.Date("2000-1-1"),by="month", length.out = 262)
model.data2_clothes <- zoo(cbind(y_clothes_2, x1_2, x2_2, x3_2), time.points)

### Model ###
# Add a local and a seasonal trend
sdy <- sd(y_clothes_2, na.rm = TRUE)
ss<- NA
ss <- list()
ss <- AddLocalLevel(ss, y_clothes_2)
ss<-AddSeasonal(ss,y_clothes_2, nseasons = 12)

# BTST model
reg.model2_clothes <- bsts(y_clothes_2 ~ ., 
                           state.specification = ss, 
                           data = model.data2_clothes, 
                           niter = 100000) # Number of runs

### Prediction ###
reg_impact2_clothes <-CausalImpact(bsts.model = reg.model2_clothes,
                                   post.period.response = post.period.response2)

### Results ###
summary(reg_impact2_clothes$model$bsts.model)
summary(reg_impact2_clothes) 
plot(reg_impact2_clothes) + theme_minimal(base_size = 10) + labs(title = "Non-Essentials Lockdown 2, Regression Model", size = 10)+
  theme(text = element_text(family = "Times New Roman"))

################################################# Display Results #########################################################
##### Summarise results #####
# Create lists with the results
abs_effect <- c(reg_impact1_food$summary$AbsEffect[1], 
                reg_impact1_clothes$summary$AbsEffect[1], 
                reg_impact1_total$summary$AbsEffect[1], 
                reg_impact2_food$summary$AbsEffect[1], 
                reg_impact2_clothes$summary$AbsEffect[1], 
                reg_impact2_total$summary$AbsEffect[1])

abs_effect_sd <- c(reg_impact1_food$summary$AbsEffect.sd[1], 
                   reg_impact1_clothes$summary$AbsEffect.sd[1], 
                   reg_impact1_total$summary$AbsEffect.sd[1], 
                   reg_impact2_food$summary$AbsEffect.sd[1], 
                   reg_impact2_clothes$summary$AbsEffect.sd[1], 
                   reg_impact2_total$summary$AbsEffect.sd[1])

rel_effect <- c(reg_impact1_food$summary$RelEffect[1], 
                reg_impact1_clothes$summary$RelEffect[1], 
                reg_impact1_total$summary$RelEffect[1], 
                reg_impact2_food$summary$RelEffect[1], 
                reg_impact2_clothes$summary$RelEffect[1], 
                reg_impact2_total$summary$RelEffect[1])

rel_effect_sd <- c(reg_impact1_food$summary$RelEffect.sd[1], 
                   reg_impact1_clothes$summary$RelEffect.sd[1], 
                   reg_impact1_total$summary$RelEffect.sd[1], 
                   reg_impact2_food$summary$RelEffect.sd[1], 
                   reg_impact2_clothes$summary$RelEffect.sd[1], 
                   reg_impact2_total$summary$RelEffect.sd[1])

p_value <- c(reg_impact1_food$summary$p[1], 
             reg_impact1_clothes$summary$p[1], 
             reg_impact1_total$summary$p[1], 
             reg_impact2_food$summary$p[1], 
             reg_impact2_clothes$summary$p[1], 
             reg_impact2_total$summary$p[1])

reg_r2_1_food <- summary(reg_impact1_food$model$bsts.model)
reg_r2_1_clothes <- summary(reg_impact1_clothes$model$bsts.model)
reg_r2_1_total <- summary(reg_impact1_total$model$bsts.model)
reg_r2_2_food <- summary(reg_impact2_food$model$bsts.model)
reg_r2_2_clothes <- summary(reg_impact2_clothes$model$bsts.model)
reg_r2_2_total <- summary(reg_impact2_total$model$bsts.model)

reg_r2 <- c(reg_r2_1_food$rsquare[1], reg_r2_1_clothes$rsquare[1], reg_r2_1_total$rsquare[1],
        reg_r2_2_food$rsquare[1], reg_r2_2_clothes$rsquare[1], reg_r2_2_total$rsquare[1])

b1 <- c(mean(reg_impact1_food$model$bsts.model$coefficients[,2]), 
        mean(reg_impact1_clothes$model$bsts.model$coefficients[,2]),
        mean(reg_impact1_total$model$bsts.model$coefficients[,2]),
        mean(reg_impact2_food$model$bsts.model$coefficients[,2]),
        mean(reg_impact2_clothes$model$bsts.model$coefficients[,2]),
        mean(reg_impact2_total$model$bsts.model$coefficients[,2]))

b2 <- c(mean(reg_impact1_food$model$bsts.model$coefficients[,3]), 
        mean(reg_impact1_clothes$model$bsts.model$coefficients[,3]),
        mean(reg_impact1_total$model$bsts.model$coefficients[,3]),
        mean(reg_impact2_food$model$bsts.model$coefficients[,3]),
        mean(reg_impact2_clothes$model$bsts.model$coefficients[,3]),
        mean(reg_impact2_total$model$bsts.model$coefficients[,3]))

b3 <- c(mean(reg_impact1_food$model$bsts.model$coefficients[,4]), 
        mean(reg_impact1_clothes$model$bsts.model$coefficients[,4]),
        mean(reg_impact1_total$model$bsts.model$coefficients[,4]),
        mean(reg_impact2_food$model$bsts.model$coefficients[,4]),
        mean(reg_impact2_clothes$model$bsts.model$coefficients[,4]),
        mean(reg_impact2_total$model$bsts.model$coefficients[,4]))


# Assign names and merge the data frames
list_names <- c("Essential Retail", "Non-Essential Retail", "Total Retail", "Essential Retail", "Non-Essential Retail", "Total Retail")
btst_output <- data.frame(list_names, abs_effect, abs_effect_sd, rel_effect, rel_effect_sd, p_value, reg_r2, b1, b2, b3)

#### Display Results #####
btst_output %>%
  gt(rowname_col =  "Name") %>%
  tab_header(
    title = md("**Analysis of Turnover in the Retail Industry**"),
    subtitle = md("*Impact of the Lockdowns in Switzerland - Regression Model*"))%>%
  tab_row_group(label = md("**Second Lockdown**"),
                rows = c(4:6))%>%
  tab_row_group(label = md("**First Lockdown**"),
                rows = c(1:3))%>%
  tab_spanner(label = md("**Absolute Effect**"), 
              columns = c(2:3))%>%
  tab_spanner(label = md("**Relative Effect**"), 
              columns = c(4:5)) %>%
  tab_spanner(label = md("**Regression Results**"), 
              columns = c(8:10)) %>%
  tab_options(
    table.width = pct(100)
  )%>%
  cols_label(abs_effect = md("*Effect*"), 
             abs_effect_sd = md("*s.d.*"), 
             rel_effect = md("*Effect*"), 
             rel_effect_sd = md("*s.d.*"), 
             p_value = md("*p-Value*"), 
             reg_r2 = md("*Prob. Causal Effect*"),
             b1 = "β1", 
             b2 = "β2",
             b3 = "β3",
             list_names = "")%>%
  cols_align(
    align = "center",
    columns = c(abs_effect, 
                abs_effect_sd, 
                rel_effect, 
                rel_effect_sd,
                p_value, 
                reg_r2,
                b1, 
                b2, 
                b3)
  )%>%
  fmt_number(
    columns = c(2),
    rows = everything(),
    decimals = 4)%>%
  fmt_number(
    columns = c(3,6,8, 9, 10),
    rows = everything(),
    decimals = 2)%>%
  fmt_percent(
    columns = c(5,7),
    decimals = 2
  )%>%
  fmt_percent(
    columns = c(4),
    decimals = 4
  )



###########################################################################################################################
######################### (Brodersen, et. al, & Scott, 2015; Scott, & Varian, 2013; Scott, 2021) #########################
###########################################################################################################################