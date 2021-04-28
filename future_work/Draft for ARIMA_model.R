#### ARIMA Attempt ####

# I tried to run use ARIMA to forecast when Hamilton's grosses will exceed The Lion King's, but came across difficulties with timeseries. I'm keeping my work here to revisit it. Feel free to build on this work.

#### data setup for ARIMA ####

# attempt 1
lion_ham_grosses <- hamilton_and_lion_king %>%
  select(week_ending, show, weekly_gross) %>%
  mutate(week_ending = as.Date(week_ending))
head(lion_ham_grosses)


lion_ham_ts <- ts(lion_ham_grosses)
class(lion_ham_ts)

# attempt 2 - splitting dataset by shows
lion_king_data <- hamilton_and_lion_king %>%
  filter(show == 'The Lion King') %>%
  select(week_ending, weekly_gross) %>%
  mutate(week_ending = as.Date(week_ending))

head(lion_king_data)

# Using extendable time series to import data to ts
lion_king_ts <- xts(lion_king_data$weekly_gross, lion_king_data$week_ending)
head(lion_king_ts)



hamilton_data <- hamilton_and_lion_king %>%
  filter(show == 'Hamilton') %>%
  select(week_ending, weekly_gross) %>%
  mutate(week_ending = as.Date(week_ending))


# attempt 3 - using sequence function as input for xts()
dates <- seq(as.Date("2015-07-19"), length =241 , by = 'weeks')

hamilton_ts <- xts(hamilton_data$weekly_gross, hamilton_data$week_ending, order.by = dates)

head(hamilton_ts,10)

frequency(hamilton_ts)

#attempt 4 - trying to reassign a frequency to Hamilton's data
hamilton_ts_f <- `frequency<-`(hamilton_ts,12)
frequency(hamilton_ts_f)

#### if setting data for ARIMA worked, use the following to explore the data ####
start(hamilton_ts)

end(hamilton_ts)

sum(is.na(hamilton_ts))

summary(hamilton_ts)

plot(hamilton_ts)



#### Decompose the Data Into Four Components ####
hamilton_ts_decomposed <- decompose(hamilton_ts, "multiplicative")
plot(hamilton_ts_decomposed)

plot(hamilton_ts_decomposed)

abline(reg=lm(hamilton_ts_decomposed~time(hamilton_ts_decomposed)))

#### Create a Box Plot by Cycle ####

#### Build the ARIMA Model Using auto.arima() Function ####

#### Plot the Residuals ####

#### Forecast the Values for the Next 5 Years ####

#### Validate the Model by Selecting Lag Values ####


