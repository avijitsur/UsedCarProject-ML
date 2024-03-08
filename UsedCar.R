##########################################################
# 2nd Capstone Project - Used Car Data analysis
##########################################################



#### Install required packages ####

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(matrixStats)
library(e1071)
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(rpart)
library(rpart.plot)
library(kableExtra)


##### Section 1: Import data #####

data <- read_csv("UsedCarData.csv")


##### Section 2: Data Exploration ####

## 2.1 Initial exploration

glimpse(data)

# check for any NA
cbind(
  lapply(
    lapply(data, is.na)
    , sum)
) 

# drop torque as not needed
data <- select(data, -torque) 

# factorizing data frame
data <- mutate(data,
                 Sales_ID = factor(Sales_ID),
                 name = factor(name),
                 Region = factor(Region),
                 `State or Province` = factor(`State or Province`),
                 City = factor(City),
                 fuel = factor(fuel),
                 seller_type = factor(seller_type),
                 transmission = factor(transmission),
                 owner = factor(owner),
                 seats = factor(seats))

# renaming columns
data <- rename(data, 
                 id = Sales_ID,
                 brand = name,
                 region = Region,
                 state_province = `State or Province`,
                 city = City)

# converting sold N,Y to 0,1

data$sold <- ifelse(data_4$sold == "Y", 1, 0) %>% factor() 


## 2.2 Summary statistics and visualization

# Summary Stats
dim(data)
summary(data)[,-1]
str(data)

# brands
n_distinct(data$brand)

# states/provinces
n_distinct(data$state_province)

# cities
n_distinct(data$city)

# Non-categorical data histograms

hist(data$selling_price, main = "", xlab = "Selling price") 
hist(data$km_driven, main = "", xlab = "Km driven")
hist(data$mileage, main = "", xlab = "Mileage") 
hist(data$engine, main = "", xlab = "Engine") 
skewness(data$engine) 
hist(data$max_power, main = "", xlab = "Max power") 
skewness((data$max_power))


# Categorical data barplots

plot(data$region, main = "", xlab = "Region") 
plot(data$state_province, main = "", xlab = "State or Province") 
plot(data$city, main = "", xlab = "City") 

plot(data$brand, main = "", xlab = "Brand") 
plot(data$fuel, main = "", xlab = "Fuel") 
plot(data$seller_type, main = "", xlab = "Seller type") 
plot(data$transmission, main = "", xlab = "Transmission") 
plot(data$owner, main = "", xlab = "Owner") 
plot(data$seats, main = "", xlab = "Seats") 
hist(data$year,main = "", xlab = "Year") 


## 2.3 Further exploration & manipulation

# Ratio of sold vs unsold cars
sold_prop <- mean(data$sold == 1) 
unsold_prop <- mean(data$sold == 0)

# Cars with 0 mileage
data %>%
  filter(mileage == 0) 

skewness(data$mileage)  

# check median mileage excluding 0's
mileage_median <- data %>%
  filter(mileage != 0) %>%
  select(mileage) %>%
  as.matrix() %>%
  colMedians(.) 

usedcardata <- data

# replace 0's with column median
usedcardata$mileage <- replace(data$mileage, data$mileage==0, mileage_median) 
summary(usedcardata$mileage)


#### Exploring ratio of sold:unsold across the predictors ####

## Categorical Variables ##

# Brand #

# variation of sold:unsold ratio across all brands
ggplot(usedcardata, aes(y = brand, fill = sold)) + geom_bar(position = "fill") 

# top brands by count
top_brand_names <- c("Maruti", "Hyundai", "Mahindra", "Tata", "Honda", "Toyota") 

top_brands <- filter(usedcardata, brand %in% top_brand_names) 

# Top 6 brands sold:unsold ratio similar to overall ratio
ggplot(top_brands, aes(y = brand, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 


# Region

ggplot(usedcardata, aes(y = region, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 


# State/Province

# variation of sold:unsold across all states/provinces
ggplot(usedcardata, aes(y = state_province, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 

# top 6
top_sp_names <- c("California", "Texas", "New York", "Illinois", "Florida", "Ohio")

top_sp <- filter(usedcardata, state_province %in% top_sp_names)

ggplot(top_sp, aes(y = state_province, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 


# City

# variation of sold:unsold across all cities

ggplot(usedcardata, aes(y = city, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 

# top 6

top_city_names <- c("New York City", "Los Angeles", "Seattle", "Chicago", "Boston", "Washington")

top_city <- filter(usedcardata, city %in% top_city_names)

ggplot(top_city, aes(y = city, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 


# Fuel Type

ggplot(usedcardata, aes(y = fuel, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 


# Transmission type

ggplot(usedcardata, aes(y = transmission, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 

# Seller type

ggplot(usedcardata, aes(y = seller_type, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 

# Owner type 

ggplot(usedcardata, aes(y = owner, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 

# No. of seats

ggplot(usedcardata, aes(y = seats, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 

# 1.3.1.10 Year

ggplot(usedcardata, aes(y = year, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") 


### Non-categorical ###

# Selling price

ggplot(usedcardata, 
       aes(x = selling_price, y = sold)) +
  geom_violin() +
  scale_x_continuous(labels = comma) +
  coord_flip() +
  geom_boxplot(width=0.2)

# Not much variation in distribution of selling price between sold and unsold cars

# Km driven

ggplot(usedcardata, 
       aes(x = km_driven, y = sold)) +
  geom_violin() +
  scale_x_continuous(labels = comma) +
  coord_flip() +
  geom_boxplot(width=0.2)

# Not much variation in distribution of km driven between sold and unsold cars


# Car mileage

ggplot(usedcardata, 
       aes(x = mileage, y = sold)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(width=0.2)

# Not much variation in distribution of mileage between sold and unsold cars


# Engine size

ggplot(usedcardata, 
       aes(x = engine, y = sold)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(width=0.2)

# Not much variation in distribution of engine size between sold and unsold cars

# Engine max power

ggplot(usedcardata, 
       aes(x = max_power, y = sold)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(width=0.2)

# Not much variation in distribution of max power between sold and unsold cars



#### 3 Modeling ####

# 3.1 Creating training and test sets

set.seed(1) 

test_index <- createDataPartition(usedcardata$sold, times = 1, p = 0.2, list = FALSE) # 20% index
test_set <- usedcardata[test_index,] # 20% test
train_set <- usedcardata[-test_index,] # 80% train

nrow(train_set)/nrow(usedcardata) # check if 80%

mean(train_set$sold == 1) # see sold % in training set, matches ratio for whole dataset

# 3.2 Basic model guessing

set.seed(2)

base <- sample(c(0,1), nrow(test_set), replace = TRUE)

mean(base == test_set$sold)

model_accuracy <- tibble(model = "Basic", accuracy = mean(base == test_set$sold))

### 3.3 Logistic regression

## By Region

set.seed(1) 

# model
train_glm_region <- train(sold ~ region, 
                          method = "glm", 
                          data = train_set)

# accuracy
pred_glm_region <- predict(train_glm_region, test_set)

cm_region_glm <- confusionMatrix(data = pred_glm_region, 
                                 reference = test_set$sold)

cm_region_glm$table
cm_region_glm$overall["Accuracy"]
cm_region_glm$byClass["F1"]

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Region GLM", accuracy = cm_region_glm$overall["Accuracy"])
model_f1 <- tibble(model = "Region GLM", f1 = cm_region_glm$byClass["F1"])


# By State or Province

set.seed(1) 

# model
train_glm_state <- train(sold ~ state_province, 
                         method = "glm", 
                         data = train_set)

# accuracy
pred_glm_state <- predict(train_glm_state, test_set)

cm_state_glm <- confusionMatrix(pred_glm_state, test_set$sold)

cm_state_glm$table
cm_state_glm$overall["Accuracy"]
cm_state_glm$byClass["F1"]

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "State/Province GLM", accuracy = cm_state_glm$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "State/Province GLM", f1 = cm_state_glm$byClass["F1"])

# By City

set.seed(1) 

# model
train_glm_city <- train(sold ~ city, 
                        method = "glm", 
                        data = train_set)

# accuracy
pred_glm_city <- predict(train_glm_city, test_set)

cm_city_glm <- confusionMatrix(pred_glm_city, test_set$sold)

cm_city_glm$table
cm_city_glm$overall["Accuracy"]
cm_city_glm$byClass["F1"] 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "City GLM", accuracy = cm_city_glm$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "City GLM", f1 = cm_city_glm$byClass["F1"])

## By Location (Region + State/Province + City)

set.seed(1) 

# model
train_glm_location <- train(sold ~ region + state_province + city, 
                            method = "glm", 
                            data = train_set)

# accuracy
pred_glm_location <- predict(train_glm_location, test_set)

cm_location_glm <- confusionMatrix(pred_glm_location, test_set$sold)

cm_location_glm$table
cm_location_glm$overall["Accuracy"] 
cm_location_glm$byClass["F1"]

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Location GLM", accuracy = cm_location_glm$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "Location GLM", f1 = cm_location_glm$byClass["F1"])

### 3.4 Decision tree

## Location (Region + State/Province +  City) 

set.seed(2) 

rpart_tunegrid <- data.frame(cp = seq(0, 0.02, 0.001))

# model
train_rpart_location <- train(sold ~ region + state_province + city, 
                              method = "rpart",
                              tuneGrid = rpart_tunegrid,
                              data = train_set)

# best cp
train_rpart_location$results %>%
  select(cp, Accuracy) %>%
  plot()

train_rpart_location$bestTune

# accuracy
pred_rpart_location <- predict(train_rpart_location, test_set)

cm_location_rpart <- confusionMatrix(pred_rpart_location, test_set$sold)

cm_location_rpart$table
cm_location_rpart$overall["Accuracy"] 
cm_location_rpart$byClass["F1"]

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Location RPART", accuracy = cm_location_rpart$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "Location RPART", f1 = cm_location_rpart$byClass["F1"])

# plot
rpart.plot(train_rpart_location$finalModel, 
           type = 2, 
           fallen.leaves = FALSE, 
           box.palette="RdGn")

## Location 2 - less complex model

# model
set.seed(2, sample.kind = "Rounding") 

train_rpart_location_2 <- train(sold ~ region + state_province + city, 
                                method = "rpart",
                                tuneGrid = data.frame(cp = seq(0.03, 0.05, 0.001)),
                                data = train_set)

# best cp
train_rpart_location_2$results %>%
  select(cp, Accuracy) %>%
  plot()

train_rpart_location_2$bestTune # 0.03

# accuracy
pred_rpart_location_2 <- predict(train_rpart_location_2, test_set)

cm_location_rpart_2 <- confusionMatrix(pred_rpart_location_2, test_set$sold)

cm_location_rpart_2$table
cm_location_rpart_2$overall["Accuracy"] # 0.8185841  
cm_location_rpart_2$byClass["F1"] #  0.8916572 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Location RPART 2", accuracy = cm_location_rpart_2$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "Location RPART 2", f1 = cm_location_rpart_2$byClass["F1"])

# plot
rpart.plot(train_rpart_location_2$finalModel, 
           type = 2, 
           fallen.leaves = FALSE, 
           box.palette="RdGn")

# Comparison of various models

kable(model_accuracy, booktabs = TRUE, caption = "Comparison of Accuracies") %>%
  kable_styling(position = "center", latex_options = c("scale_down", "striped"))

kable(model_f1, booktabs = TRUE, caption = "Comparison of F1 Score") %>%
  kable_styling(position = "center", latex_options = c("scale_down", "striped"))

