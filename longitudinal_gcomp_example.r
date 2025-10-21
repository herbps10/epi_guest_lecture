library(tidyverse)
library(ranger)

simulate <- function(N = 1e2, counterfactual = FALSE) {
  L0 <- runif(N, 0, 1)
  if(counterfactual == FALSE) {
    A0 <- rbinom(N, 1, 0.5)
  }
  else {
    A0 <- rep(1, N)
  }
  
  L1 <- 0.5 * A0 + runif(N, -0.1, 0.1)
  if(counterfactual == FALSE) {
    A1 <- rbinom(N, 1, 0.5)
  }
  else {
    A1 <- rep(1, N)
  }
  
  L2 <- 0.5 * A1 + runif(N, -0.1, 0.1)
  if(counterfactual == FALSE) {
    A2 <- rbinom(N, 1, 0.5)
  }
  else {
    A2 <- rep(1, N)
  }
  
  Y <- rnorm(N, L2, 0.05)
  
  data.frame(L0, A0, L1, A1, L2, A2, Y)
}

# Calculate true parameter value
mean(simulate(1e5, counterfactual = TRUE)$Y)

N <- 50
data <- simulate(N)

# First, try fitting a normal regression model...
fit <- ranger(Y ~ L0 + L1 + L2 + A0 + A1 + A2, data = data)

# And make predictions under the scenario where A0 = 1, A1 = 1, and A2 = 1.
predictions <- predict(fit, data = mutate(data, A0 = 1, A1 = 1, A2 = 1))$predictions

# The mean of these predictions does not equal the true parameter value!
mean(predictions)

# To get the correct estimate, we need to apply the longitudinal g-formula.

# Step 1: fit a model regressing outcome Y on all variables
fit1 <- ranger(Y ~ L0 + L1 + L2 + A0 + A1 + A2, data = data)
# Predict the outcome in a modified dataset with A0 = 1, A1 = 1, A2 = 1.
data$prediction1 <- predict(fit1, data = mutate(data, A0 = 1, A1 = 1, A2 = 1))$predictions

# Step 2: fit a model regressing predictions from step 1 on all variables up to time point 2.
fit2 <- ranger(prediction1 ~ L0 + A0 + L1 + A1, data = data)
# Predict with a modified dataset with A0 = 1, A1 = 1.
data$prediction2 <- predict(fit2, data = mutate(data, A0 = 1, A1 = 1))$predictions

# Step 3: fit a model regressing predictions from step 2 on variables from time point 1.
fit3 <- ranger(prediction2 ~ L0 + A0, data = data)
# Predict with modified dataset with A0 = 1.
data$prediction3 <- predict(fit3, data = mutate(data, A0 = 1))$predictions

# Now overage the final predictions from Step 3.
mean(data$prediction3)


#### Plots ####
ylim <- range(c(data$L0, data$L1, data$L2, data$Y))

# Step 1
ggplot(data, aes(x = 1, y = L0)) +
  geom_segment(aes(x = 1, xend = 2, y = L0, yend = L1), color = "gray") +
  geom_segment(aes(x = 2, xend = 3, y = L1, yend = L2), color = "gray") +
  geom_segment(aes(x = 3, xend = 4, y = L2, yend = Y), color = "gray") +
  geom_point(aes(color = factor(A0))) +
  geom_point(aes(x = 2, y = L1, color = factor(A1, levels = c(0, 1)))) +
  geom_point(aes(x = 3, y = L2, color = factor(A2, levels = c(0, 1)))) +
  geom_point(aes(x = 4, y = Y)) +
  labs(x = "Time point", y = "Value", title = "Regress Y on history") +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))

data |> 
  mutate(A0 = 1, A1 = 1, A2 = 1) |>
  ggplot(aes(x = 1, y = L0)) +
  geom_segment(aes(x = 1, xend = 2, y = L0, yend = L1), color = "gray") +
  geom_segment(aes(x = 2, xend = 3, y = L1, yend = L2), color = "gray") +
  geom_segment(aes(x = 3, xend = 4, y = L2, yend = prediction1), color = "gray") +
  geom_point(aes(color = factor(A0, levels = c(0, 1)))) +
  geom_point(aes(x = 2, y = L1, color = factor(A1, levels = c(0, 1)))) +
  geom_point(aes(x = 3, y = L2, color = factor(A2, levels = c(0, 1)))) +
  geom_point(aes(x = 4, y = prediction1)) +
  labs(x = "Time point", y = "Value", title = "Predict with A0 = 1, A1 = 1, A2 = 1") +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))

# Step 2
data |> 
  ggplot(aes(x = 1, y = L0)) +
  geom_segment(aes(x = 1, xend = 2, y = L0, yend = L1), color = "gray") +
  geom_segment(aes(x = 2, xend = 3, y = L1, yend = prediction1), color = "gray") +
  geom_point(aes(color = factor(A0))) +
  geom_point(aes(x = 2, y = L1, color = factor(A1, levels = c(0, 1)))) +
  geom_point(aes(x = 3, y = prediction1)) +
  labs(x = "Time point", y = "Value", title = "Regress predicted value on history") +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))

data |> 
  mutate(A0 = 1, A1 = 2) |>
  ggplot(aes(x = 1, y = L0)) +
  geom_segment(aes(x = 1, xend = 2, y = L0, yend = L1), color = "gray") +
  geom_segment(aes(x = 2, xend = 3, y = L1, yend = prediction2), color = "gray") +
  geom_point(aes(color = factor(A0, levels = c(0, 1)))) +
  geom_point(aes(x = 2, y = L1, color = factor(A0))) +
  geom_point(aes(x = 3, y = prediction2)) +
  labs(x = "Time point", y = "Value", title = "Predicted value with A0 = 1, A1 = 1") +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))
    
# Step 3

data |> 
  ggplot(aes(x = 1, y = L0)) +
  geom_segment(aes(x = 1, xend = 2, y = L0, yend = prediction2), color = "gray") +
  geom_point(aes(color = factor(A0, levels = c(0, 1)))) +
  geom_point(aes(x = 2, y = prediction2)) +
  labs(x = "Time point", y = "Value", title = "Regress predicted value on history") +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))

data |> 
  mutate(A0 = 1) |>
  ggplot(aes(x = 1, y = L0)) +
  geom_segment(aes(x = 1, xend = 2, y = L0, yend = prediction3), color = "gray") +
  geom_point(aes(color = factor(A0, levels = c(0, 1)))) +
  geom_point(aes(x = 2, y = prediction3)) +
  labs(x = "Time point", y = "Value", title = "Predicted value with A0 = 1") +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))

# Step 4

data |>
  mutate(A0 = 1) |>
  ggplot(aes(x = 1, y = prediction3)) +
  geom_point(aes(color = factor(A0, levels = c(0, 1)))) +
  labs(x = "Time point", y = "Value", title = "Predicted value with A0 = 1") +
  scale_x_continuous(breaks = c(1:4), limits = c(1, 4)) +
  scale_color_manual(values = c("blue", "red"), drop = FALSE) +
  scale_y_continuous(limits = ylim) +
  guides(color = guide_legend("Treatment"))
  
mean(data$prediction3)

