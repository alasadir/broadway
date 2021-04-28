# An Extension to the Model in the Case that data is split into training and testing sets
# There are errors with this code that needs to be fixed

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

#plotting
hamilton_and_lion_king %>%
  ggplot(aes(x = avg_ticket_price, seats_sold, y = top_ticket_price, color = show)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Average Ticket Price (USD) + Seats Sold",
       y = "Top Ticket Price (USD)",
       color = "Show") +
  theme_classic()

# residuals
top_model_res <- residuals(top_model)
top_model_res <- as.data.frame(top_model_res)
plot(top_model_res)
broom::augment(top_model)

# top model results (predicted vs. actual)
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