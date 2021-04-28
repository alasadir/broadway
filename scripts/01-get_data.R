#### Preamble ####
# Purpose: Build and run a multiple regression to forecast average and top ticket prices for The Lion King and Hamilton
# Author: Reem Alasadi
# Data: 26 April 2021
# Contact: reem.alasadi@mail.utoronto.ca
# License: MIT



#### Workspace setup ####

library(tidyverse)
library(lubridate)
library(kableExtra)
library(broom)
library(huxtable)
library(tidymodels)


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

#### Data Exploration ####

#preview dataset
head(hamilton_and_lion_king, 1) %>%
  knitr::kable(caption = "", digits = 2) %>%
  kableExtra::kable_styling(full_width = FALSE, latex_options = "scale_down")

#show column names
colnames(hamilton_and_lion_king)

#summary statistics
summary(hamilton_and_lion_king) %>%
  knitr::kable(caption = "Dataset Summary", digits = 2) %>%
  kableExtra::kable_styling(full_width = FALSE)

#view null values
sum(is.na(hamilton_and_lion_king))

#plotting grosses over the years
hamilton_and_lion_king %>% 
  ggplot(aes(x = week_ending, y = weekly_gross/1000000)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Weekly Gross (in millions)") +
  xlab("") +
  theme_minimal()

#plotting performances per week
hamilton_and_lion_king %>% 
  mutate(performances = as.integer(performances)) %>% 
  ggplot(aes(x = week_ending, y = performances)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Number of Performances") +
  xlab("") +
  theme_minimal()

#plotting seats sold per week versus average ticket price
hamilton_and_lion_king %>% 
  ggplot(aes(x = avg_ticket_price, y = seats_sold)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Seats Sold per Week") +
  xlab("Average Ticket Price (USD)") +
  theme_minimal()

#Percentage of theater capacity sold versus average ticket price
hamilton_and_lion_king %>% 
  ggplot(aes(x = avg_ticket_price, y = pct_capacity*100)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Theatre Capacity (%)") +
  xlab("Average Ticket Price (USD)") +
  theme_minimal()

#Average ticket prices each week
hamilton_and_lion_king %>% 
  ggplot(aes(x = week_ending, y = avg_ticket_price)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Average Ticket Price (USD)") +
  xlab("") +
  theme_minimal()

#Top ticket prices per week
hamilton_and_lion_king %>% 
  ggplot(aes(x = week_ending, y = top_ticket_price)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Top Ticket Price (USD)") +
  xlab("") +
  theme_minimal()

#Top ticket prices versus average ticket prices per year
hamilton_and_lion_king %>% 
  mutate(year_of_show = lubridate::year(week_ending)) %>%
  filter(year_of_show %in% c("2015", "2016", "2017", "2018", "2019", "2020")) %>%
  ggplot(aes(x = avg_ticket_price, y = top_ticket_price)) +
  geom_point(aes(color = show), alpha = 0.8) +
  ylab("Top Ticket Price (USD)") +
  xlab("Average Ticket Price (USD)") +
  theme_minimal() +
  facet_wrap(vars(year_of_show))

#### Model: Multiple Linear Regression ####

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

## Top Ticket Price Model ##

#creating multiple regression for top ticket price
top_model <- lm(formula = top_ticket_price ~
                  avg_ticket_price +
                  seats_sold + 
                  show,  
                data = hamilton_and_lion_king)
summary(top_model)

#plotting top_model
hamilton_and_lion_king %>%
  ggplot(aes(x = avg_ticket_price, seats_sold, y = top_ticket_price, color = show)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Average Ticket Price (USD) + Seats Sold",
       y = "Top Ticket Price (USD)",
       color = "Show") +
  theme_classic()

#Plotting Top Model Residuals

#getting the residuals
broom::augment(top_model)

ggplot(top_model, 
       aes(x = .resid)) + 
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(y = "Number of occurrences",
       x = "Residuals")

#Top Model Confidence Intervals

##some exploration first
glance(top_model)

tidy(top_model)

## using the confidence interval function
confint(top_model)

#plotting confidence interval
set.seed(1407)

top_model_intervals <- 
  reg_intervals(top_ticket_price ~ avg_ticket_price + seats_sold + show, 
                data = model_data,
                keep_reps = TRUE,
                model_fn = "lm")
top_model_intervals

top_model_intervals%>% 
  select(term, .replicates) %>% 
  unnest(cols = .replicates) %>% 
  ggplot(aes(x = estimate)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~ term, scales = "free_x") + 
  geom_vline(data = top_model_intervals, aes(xintercept = .lower), col = "red") + 
  geom_vline(data = top_model_intervals, aes(xintercept = .upper), col = "red") + 
  geom_vline(xintercept = 0, col = "green")

#### Average Ticket Price Model ####

#creating multiple regression for avg ticket price
avg_model <- lm(formula = avg_ticket_price ~
                  top_ticket_price +
                  seats_sold + 
                  show,  
                data = hamilton_and_lion_king)
summary(avg_model)

#plotting avg_model
hamilton_and_lion_king %>%
  ggplot(aes(x = top_ticket_price, seats_sold, y = avg_ticket_price, color = show)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Top Ticket Price (USD) + Seats Sold",
       y = "Average Ticket Price (USD)",
       color = "Show") +
  theme_classic()

#getting Avg Model residuals
broom::augment(avg_model)

#plotting Avg Model residuals
ggplot(avg_model, 
       aes(x = .resid)) + 
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(y = "Number of occurrences",
       x = "Residuals")


#avg model diagnostics
broom::glance(avg_model)


#avg model coefficient estimates
broom::tidy(avg_model)

#Avg Model Confidence Intervals
confint(avg_model)

#avg model confidence interval prep
set.seed(1407)

avg_model_intervals <- 
  reg_intervals(avg_ticket_price ~ top_ticket_price + seats_sold + show, 
                data = model_data,
                keep_reps = TRUE,
                model_fn = "lm")
avg_model_intervals

#plotting avg model confidence intervals
avg_model_intervals%>% 
  select(term, .replicates) %>% 
  unnest(cols = .replicates) %>% 
  ggplot(aes(x = estimate)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~ term, scales = "free_x") + 
  geom_vline(data = avg_model_intervals, aes(xintercept = .lower), col = "red") + 
  geom_vline(data = avg_model_intervals, aes(xintercept = .upper), col = "red") + 
  geom_vline(xintercept = 0, col = "green")

#displaying regression models side-by-side
huxreg(top_model, avg_model)

