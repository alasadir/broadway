#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Reem Alasadi
# Data: 26 April 2021
# Contact: reem.alasadi@mail.utoronto.ca
# License: MIT



#### Workspace setup ####

library(tidyverse)
library(lubridate)
library(kableExtra)
library(broom)
library(performance)
library(see)
library(qqplotr)
library(huxtable)
library(tidymodels)
library(rstanarm)

# Read in the raw data

#grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
grosses <- read_csv(here::here("inputs/data/grosses.csv"),
                    guess_max = 40000)

# Save data
write_csv(grosses, "inputs/data/grosses.csv")

# Filter data by show
hamilton_and_lion_king <- 
  grosses %>% 
  filter(show %in% c("Hamilton", "The Lion King"))

# Save dataset
write_csv(hamilton_and_lion_king, "inputs/data/hamilton_and_lion_king.csv")


#### Model ####

#Calculating Correlation between Variables Using Pearson's Method

## 1
cor(hamilton_and_lion_king$seats_sold, hamilton_and_lion_king$avg_ticket_price, method = "pearson")

## 2
cor(hamilton_and_lion_king$seats_sold, hamilton_and_lion_king$top_ticket_price, method = "pearson")

## 3
cor(hamilton_and_lion_king$top_ticket_price, hamilton_and_lion_king$avg_ticket_price, method = "pearson")


#### Multiple Linear Regression ####

#since 'show' column contains binary data, Lion King & Hamilton, it is considered as categorical data and should be converted to a factor
hamilton_and_lion_king <-
  hamilton_and_lion_king %>%
  mutate(show = as_factor(show))

#checking the data type for 'show' to make sure changes were made
class(hamilton_and_lion_king$show)

#checking the order of the shows within the column. This will help determine which show will be taken as a dependent variable in the lm() model
levels(hamilton_and_lion_king$show)

# Selecting model variables
model_data <- hamilton_and_lion_king %>%
  select(top_ticket_price,
         avg_ticket_price,
         seats_sold,
         show)

# Checking summary statistics
summary(model_data)
         
# Checking dataset dimensions
dim(model_data)

# Plotting variables
plot(model_data)

# Splitting data into training and testing sets
set.seed(2)

ham_lion_split <- rsample::initial_split(model_data, prop = 0.80)
ham_lion_split

ham_lion_train <- rsample::training(ham_lion_split)
ham_lion__test  <-  rsample::testing(ham_lion_split)

# modeling the training samples
top_model <- lm(top_ticket_price ~., data = ham_lion_train)
summary(top_model)

avg_model <- lm(avg_ticket_price ~., data = ham_lion_train)
summary(avg_model)
broom::tidy(top_model)

# modeling the test samples
top_model_pred <- predict(top_model, ham_lion__test)
plot(top_model_pred)


# residuals
top_model_res <- residuals(top_model)
top_model_res <- as.data.frame(top_model_res)
plot(top_model_res)
broom::augment(top_model)

# top model results
top_model_results <- cbind(top_model_pred, ham_lion_test$top_ticket_price)

# viewing models side-by-side
huxreg(top_model, avg_model)

#plotting top_model}
hamilton_and_lion_king %>%
  ggplot(aes(x = avg_ticket_price, seats_sold, y = top_ticket_price, color = show)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Average Ticket Price (USD) + Seats Sold",
       y = "Top Ticket Price (USD)",
       color = "Show") +
  theme_classic()'''