###########################################################################################################################
######################################################### Model 1 #########################################################
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

################################################ Variable Definition #######################################################
##### General Variables #####
### Define a pre- and a post-period
pre.period <- c(1, 240)   
post.period1 <- c(243,252) # Post-period for the first Lockdown
post.period2 <- c(253,262) # Post-period for the second Lockdown

ld1 <- subset(revenue_brut, Date < "2021-01-01") # Data subset for the first lockdown
ld2 <- subset(revenue_brut, Date < "2021-11-01") # Data subset for the second lockdown

### Create a copy for the post period
ld1_2 <- ld1
ld2_2 <- ld2

################################################# Model Lockdown 1 ########################################################
##### Model 1.1 - Total Retail #####
# Assign the pre- and a post-period
post.period.response1_total <- ld1$Total_Retail[post.period1[1] : post.period1[2]]

# Remove the outcome from the post-period 
ld1_2$Total_Retail[post.period1[1] : post.period1[2]] <- NA

# Create a zoo object
revenue_zoo1_total <- zoo(ld1_2$Total_Retail, ld1_2$Date)

### Model ###
# Add a local and a seasonal trend
ss1_total <- AddLocalLevel(list(), as.numeric(revenue_zoo1_total))
ss1_total <- AddSeasonal(ss1_total, as.numeric(revenue_zoo1_total), nseasons = 12) 

# BTST model
bsts.model1_total <- bsts(revenue_zoo1_total, 
                          state.specification = ss1_total,
                          niter = 100000, # Number of runs
                          burn = 500) # Burn In rate

### Prediction ###
impact1_total <- CausalImpact(bsts.model = bsts.model1_total,
                               post.period.response = as.numeric(post.period.response1_total))

### Results ###
summary(impact1_total)
summary(impact1_total, "report")
plot(impact1_total) + theme_minimal(base_size = 10) + labs(title = "Total Lockdown 1", size = 10)+
  theme(text = element_text(family = "Times New Roman"))

##### Model 1.2 - Essential Retail #####
# Assign the pre- and a post-period
post.period.response1_food <- ld1$Food_Drinks_Tobacco[post.period1[1] : post.period1[2]]

# Remove the outcome from the post-period 
ld1_2$Food_Drinks_Tobacco[post.period1[1] : post.period1[2]] <- NA

# Create a zoo object
revenue_zoo1_food <- zoo(ld1_2$Food_Drinks_Tobacco, ld1_2$Date)

### Model ###
# Add a local and a seasonal trend
ss1_food <- AddLocalLevel(list(), as.numeric(revenue_zoo1_food))
ss1_food <- AddSeasonal(ss1_food, as.numeric(revenue_zoo1_food), nseasons = 12) 

# BTST model
bsts.model1_food <- bsts(revenue_zoo1_food, 
                         state.specification = ss1_food,
                         niter = 100000, # Number of runs
                         burn = 500) # Burn In rate

### Prediction ###
impact1_food <- CausalImpact(bsts.model = bsts.model1_food,
                              post.period.response = as.numeric(post.period.response1_food))

### Results ###
summary(impact1_food)
summary(impact1_food, "report")
plot(impact1_food) + theme_minimal(base_size = 10) + labs(title = "Essentials Lockdown 1", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


##### Model 1.3 - Non-Essential Retail #####
# Assign a pre- and a post-period
post.period.response1_clothes <- ld1$Clothes_Shoes[post.period1[1] : post.period1[2]]

# Remove the outcome from the post-period 
ld1_2$Clothes_Shoes[post.period1[1] : post.period1[2]] <- NA

# Create a zoo object
revenue_zoo1_clothes <- zoo(ld1_2$Clothes_Shoes, ld1_2$Date)

### Model ###
# Add a local and a seasonal trend
ss1_clothes <- AddLocalLevel(list(), as.numeric(revenue_zoo1_clothes))
ss1_clothes <- AddSeasonal(ss1_clothes, as.numeric(revenue_zoo1_clothes), nseasons = 12) 

# BTST model
bsts.model1_clothes <- bsts(revenue_zoo1_clothes, 
                            state.specification = ss1_food,
                            niter = 100000, # Number of runs
                            burn = 500) # Burn In rate

### Prediction ###
impact1_clothes <- CausalImpact(bsts.model = bsts.model1_clothes,
                                 post.period.response = as.numeric(post.period.response1_clothes))

### Results ###
summary(impact1_clothes)
summary(impact1_clothes, "report")
plot(impact1_clothes) + theme_minimal(base_size = 10) + labs(title = "Non-Essentials Lockdown 1", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


################################################# Model Lockdown 2 ########################################################
##### Model 1.4 - Total Retail #####
# Assign the pre- and a post-period
post.period.response2_total <- ld2$Total_Retail[post.period2[1] : post.period2[2]]

# Remove the outcome from the post-period 
ld2_2$Total_Retail[post.period2[1] : post.period2[2]] <- NA

# Create a zoo object
revenue_zoo2_total <- zoo(ld2_2$Total_Retail, ld2_2$Date)

### Model ###
# Add a local and a seasonal trend
ss2_total <- AddLocalLevel(list(), as.numeric(revenue_zoo2_total))
ss2_total <- AddSeasonal(ss2_total, as.numeric(revenue_zoo2_total), nseasons = 12) 

# BTST model
bsts.model2_total <- bsts(revenue_zoo2_total, 
                          state.specification = ss2_total,
                          niter = 100000, # Number of runs
                          burn = 500) # Burn In rate

### Prediction ###
impact2_total <- CausalImpact(bsts.model = bsts.model2_total,
                              post.period.response = as.numeric(post.period.response2_total))

### Results ###
summary(impact2_total)
summary(impact2_total, "report")
plot(impact2_total) + theme_minimal(base_size = 10) + labs(title = "Total Lockdown 2", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


##### Model 1.5 - Essential Retail #####
# Assign the pre- and a post-period
post.period.response2_food <- ld2$Food_Drinks_Tobacco[post.period2[1] : post.period2[2]]

# Remove the outcome from the post-period 
ld2_2$Food_Drinks_Tobacco[post.period2[1] : post.period2[2]] <- NA

# Create a zoo object
revenue_zoo2_food <- zoo(ld2_2$Food_Drinks_Tobacco, ld2_2$Date)

### Model ###
# Add a local and a seasonal trend
ss2_food <- AddLocalLevel(list(), as.numeric(revenue_zoo2_food))
ss2_food <- AddSeasonal(ss2_food, as.numeric(revenue_zoo2_food), nseasons = 12) 

# BTST model
bsts.model2_food <- bsts(revenue_zoo2_food, 
                         state.specification = ss2_food,
                         niter = 100000, # Number of runs
                         burn = 500) # Burn In rate

### Prediction ###
impact2_food <- CausalImpact(bsts.model = bsts.model2_food,
                             post.period.response = as.numeric(post.period.response2_food))

### Results ###
summary(impact2_food)
summary(impact2_food, "report")
plot(impact2_food) + theme_minimal(base_size = 10) + labs(title = "Essentials Lockdown 2", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


##### Model 1.6 - Non-Essential Retail #####
# Assign a pre- and a post-period
post.period.response2_clothes <- ld2$Clothes_Shoes[post.period2[1] : post.period2[2]]

# Remove the outcome from the post-period 
ld2_2$Clothes_Shoes[post.period2[1] : post.period2[2]] <- NA

# Create a zoo object
revenue_zoo2_clothes <- zoo(ld2_2$Clothes_Shoes, ld2_2$Date)

### Model ###
# Add a local and a seasonal trend
ss2_clothes <- AddLocalLevel(list(), as.numeric(revenue_zoo2_clothes))
ss2_clothes <- AddSeasonal(ss2_clothes, as.numeric(revenue_zoo2_clothes), nseasons = 12) 

# BTST model
bsts.model2_clothes <- bsts(revenue_zoo2_clothes, 
                            state.specification = ss2_food,
                            niter = 100000, # Number of runs
                            burn = 500) # Burn In rate

### Prediction ###
impact2_clothes <- CausalImpact(bsts.model = bsts.model2_clothes,
                                post.period.response = as.numeric(post.period.response2_clothes))

### Results ###
summary(impact2_clothes)
summary(impact2_clothes, "report")
plot(impact2_clothes) + theme_minimal(base_size = 10) + labs(title = "Non-Essentials Lockdown 2", size = 10)+
  theme(text = element_text(family = "Times New Roman"))


################################################# Display Results #########################################################
##### Summarise results #####
# Create lists with the results
abs_effect <- c(round(impact1_food$summary$AbsEffect[1], digits = 2), 
                 impact1_clothes$summary$AbsEffect[1], 
                 impact1_total$summary$AbsEffect[1], 
                 impact2_food$summary$AbsEffect[1], 
                 impact2_clothes$summary$AbsEffect[1], 
                 impact2_total$summary$AbsEffect[1])

abs_effect_sd <- c(impact1_food$summary$AbsEffect.sd[1], 
                   impact1_clothes$summary$AbsEffect.sd[1], 
                   impact1_total$summary$AbsEffect.sd[1], 
                   impact2_food$summary$AbsEffect.sd[1], 
                   impact2_clothes$summary$AbsEffect.sd[1], 
                   impact2_total$summary$AbsEffect.sd[1])

rel_effect <- c(impact1_food$summary$RelEffect[1], 
                impact1_clothes$summary$RelEffect[1], 
                impact1_total$summary$RelEffect[1], 
                impact2_food$summary$RelEffect[1], 
                impact2_clothes$summary$RelEffect[1], 
                impact2_total$summary$RelEffect[1])

rel_effect_sd <- c(impact1_food$summary$RelEffect.sd[1], 
                   impact1_clothes$summary$RelEffect.sd[1], 
                   impact1_total$summary$RelEffect.sd[1], 
                   impact2_food$summary$RelEffect.sd[1], 
                   impact2_clothes$summary$RelEffect.sd[1], 
                   impact2_total$summary$RelEffect.sd[1])

p_value <- c(impact1_food$summary$p[1], 
                   impact1_clothes$summary$p[1], 
                   impact1_total$summary$p[1], 
                   impact2_food$summary$p[1], 
                   impact2_clothes$summary$p[1], 
                   impact2_total$summary$p[1])

r2_1_food <- summary(impact1_food$model$bsts.model)
r2_1_clothes <- summary(impact1_clothes$model$bsts.model)
r2_1_total <- summary(impact1_total$model$bsts.model)
r2_2_food <- summary(impact2_food$model$bsts.model)
r2_2_clothes <- summary(impact2_clothes$model$bsts.model)
r2_2_total <- summary(impact2_total$model$bsts.model)

r2 <- c(r2_1_food$rsquare[1], r2_1_clothes$rsquare[1], r2_1_total$rsquare[1],
        r2_2_food$rsquare[1], r2_2_clothes$rsquare[1], r2_2_total$rsquare[1])


# Assign names and merge the data frames
list_names <- c("Essential Retail", "Non-Essential Retail", "Total Retail", "Essential Retail", "Non-Essential Retail", "Total Retail")
btst_output <- data.frame(list_names, abs_effect, abs_effect_sd, rel_effect, rel_effect_sd, p_value, r2)

#### Display Results #####
btst_output %>%
  gt(rowname_col =  "Name") %>%
  tab_header(
    title = md("**Analysis of Turnover in the Retail Industry**"),
    subtitle = md("*Impact of the Lockdowns in Switzerland - Standard Model*"))%>%
  tab_row_group(label = md("**Second Lockdown**"),
                rows = c(4:6))%>%
  tab_row_group(label = md("**First Lockdown**"),
                rows = c(1:3))%>%
  tab_spanner(label = md("**Absolute Effect**"), 
              columns = c(2:3))%>%
  tab_spanner(label = md("**Relative Effect**"), 
              columns = c(4:5)) %>%
  tab_options(
    table.width = pct(100)
  )%>%
  cols_label(abs_effect = md("*Effect*"), 
             abs_effect_sd = md("*s.d.*"), 
             rel_effect = md("*Effect*"), 
             rel_effect_sd = md("*s.d.*"), 
             r2 = md("*Prob. Causal Effect*"),
             p_value = md("*p-Value*"), 
             list_names = "")%>%
  cols_align(
    align = "center",
    columns = c(abs_effect, 
                abs_effect_sd, 
                rel_effect, 
                rel_effect_sd,
                p_value,
                r2)
  )%>%
  fmt_number(
    columns = c(2),
    rows = everything(),
    decimals = 4)%>%
  fmt_number(
    columns = c(3, 6),
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