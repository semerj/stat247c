library(multcomp)
rm(list=ls())

assignment1 = function(obs) {
  set.seed(247)
  X1 = 5 * runif(n = obs)
  X2 = 0.5 * X1 * rnorm(n = obs, mean = 0, sd = 2)
  b0 = -2
  b1 = 2
  b2 = -2.25
  logitPY = b0 + b1*X1 + b2*X2
  PY = 1/(1 + exp(-logitPY))
  Y = rbinom(n = obs, size = 1, prob = PY)
  pred_prob = 1/(1 + exp(-(b0 + b1*0 + b2*1)))

  # Lowess plots (smooth) on logit scale
  logitPX1 = loess(Y ~ X1)
  logitPX2 = loess(Y ~ X2)

  plot(logitPX1)
  plot(logitPX2)

  # transform to probability
  PX1 = 1/(1+exp(-predict(logitPX1, X1)))
  PX2 = 1/(1+exp(-predict(logitPX2, X2)))

  # Plots
  par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
  plot(X1, PX1, ylab="P(Y=1|X1=x1)", xlab="x1", main="Estimate P(Y=1|X1=x1)")
  plot(X2, PX2, ylab="P(Y=1|X2=x2)", xlab="x2", main="Estimate P(Y=1|X2=x2)")
  mtext("Lowess Smooths", outer = TRUE, cex = 1.5)

  # Run logistic regression
  logreg = glm(Y ~ X1 + X2, family = "binomial")

  # Get odds ratios from model
  print(exp(cbind(OR = coef(logreg), confint(logreg))))
  print(summary(logreg))

  mod <- glht(logreg, linfct = c("0.5 * X1 = 0"))
  confint(mod)
}

assignment1(100)