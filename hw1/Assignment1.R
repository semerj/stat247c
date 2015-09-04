obs = 100
X1 = 5 * runif(n = obs)
X2 = 0.5 * X1 * rnorm(n = obs, mean = 0, sd = 2)
b0 = -2
b1 = 2
b2 = -2.25
logitPY = b0 + b1*X1 + b2*X2
PY = 1/(1 + exp(-logitPY))
Y = rbinom(n = obs, size = 1, prob = PY)
1/(1 + exp(-(b0 + b1*0 + b2*1)))

# Lowess plots (smooth) on logit scalee
logitPX1 = glm(Y ~ X1, family = "binomial")
logitPX2 = glm(Y ~ X2, family = "binomial")

# transform to probability
PX1 = 1/(1+exp(-predict(logitPX1)))
PX2 = 1/(1+exp(-predict(logitPX2)))
