simulate <- function(N = 1e2, alpha = 1) {
  # Generate covariate
  X <- rnorm(N)
  
  # Generate treatment
  A <- rbinom(N, 1, plogis(alpha * X))
  
  # Generate counterfactual outcome
  # `plogis` is used to make sure that Y is in between 0 and 1
  Y0 <- plogis(rnorm(N, mean = X, sd = 0.1)) 
  Y1 <- plogis(rnorm(N, mean = X + 1, sd = 0.1)) 
  
  # Observed outcome
  Y <- ifelse(A == 1, Y1, Y0)
  
  data.frame(X, A, Y1, Y0, Y)
}

N <- 100
alpha <- 3 # Higher values of alpha lead to more extreme positivity violations

# Estimate the true average treatment effect by taking a very large
# sample and then taking the average difference between the counterfactual outcomes
true_ate <- with(simulate(1e5), mean(Y1 - Y0))

data <- simulate(N, alpha)

# Plot generated data
plot(data$X, data$Y, xlab = "X", ylab = "Y", col = ifelse(data$A == 1, "red", "black"), ylim = c(0, 1))
legend("bottomright", c("A = 1", "A = 0"), fill = c("red", "black"))

# Estimate propensity score model
g_model <- glm(A ~ X, data = data, family = "binomial")
g_hat   <- predict(g_model, type = "response")

# Plot estimated propensity scores
plot(data$X, g_hat, xlab = "X", ylab = "Estimated propensity score")
hist(g_hat, xlab = "Estimated propensity score")
hist(1 / g_hat, xlab = "Estimated inverse propensity score")

# Estimate outcome regression model
m_model <- glm(Y ~ A + X, data = data, family = "binomial")
m_hat   <- predict(m_model, type = "response")
m0_hat  <- predict(m_model, type = "response", newdata = transform(data, A = 0))
m1_hat  <- predict(m_model, type = "response", newdata = transform(data, A = 1))

# Plot outcome regression predictions
plot(data$X, data$Y, xlab = "X", ylab = "Y", col = ifelse(data$A == 1, "red", "black"), ylim = c(0, 1))
legend("bottomright", c("A = 1", "A = 0"), fill = c("red", "black"))
lines(data$X[order(data$X)], m1_hat[order(data$X)], col = "red", lty = 2)
lines(data$X[order(data$X)], m0_hat[order(data$X)], col = "black", lty = 2)

#
# Calculate IPTW, G-computation, and Augmented IPTW estimates
#

unadjusted <- with(data, mean(Y[A == 1]) - mean(Y[A == 0]))

iptw_weights <- with(data, (A == 1) / g_hat - (A == 0) / (1 - g_hat))
iptw <- mean(iptw_weights * data$Y)

gcomp <- mean(m1_hat - m0_hat)

aiptw <- with(data, mean(m1_hat - m0_hat + iptw_weights * (Y - m_hat)))


# Look at results
true_ate
unadjusted
iptw
gcomp
aiptw

# Form confidence interval

D <- with(data, m1_hat - m0_hat + (A / g_hat - (1 - A) / (1 - g_hat)) * (Y - m_hat))
se <- sd(D) / sqrt(N)
ci <- aiptw + qnorm(c(0.025, 0.975)) * se
ci

# Targeted Minimum Loss-Based Estimation
clever1 <- with(data, A / g_hat)
clever0 <- with(data, -(1 - A) / (1 - g_hat))
clever <- with(data, A / g_hat - (1 - A) / (1 - g_hat))

fluctuation_model <- glm(Y ~ -1 + clever + offset(qlogis(m_hat)), data = data, family = "binomial")
epsilon <- coef(fluctuation_model)

m1_star <- plogis(qlogis(m1_hat) + epsilon * clever1)
m0_star <- plogis(qlogis(m0_hat) + epsilon * clever0)
tmle <- mean(m1_star - m0_star)

true_ate
unadjusted
iptw
gcomp
aiptw
tmle

