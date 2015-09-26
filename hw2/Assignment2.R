# Data-generating distributions for repeated measures data
# Question 1.4 in chapter 1 (Jewell and Hubbard).

set.seed(247)

assignment2 = function(n=5000, numlist=c(20, 100, 1000)) {
  id = 1:n
  mu = 10
  rho = 0.3
  sigma_alpha = sqrt(0.5)
  sigma_e = sqrt((sigma_alpha^2/rho) - sigma_alpha^2)

  alpha = rnorm(n, 0, sigma_alpha)
  e1 = rnorm(n, 0, sigma_e)
  e2 = rnorm(n, 0, sigma_e)

  Y_time1 = mu + alpha + e1
  Y_time2 = mu + alpha + e2

  hist(c(Y_time1, Y_time2))

  cor = sigma_alpha^2/(sigma_alpha^2 + sigma_e^2)
  cat("cor = ", cor, "\n")

  # combine data into wide format
  df = data.frame(id, alpha, e1, e2, Y_time1, Y_time2)

  mu_est_vec = NULL
  rho_est_vec = NULL
  for(i in 1:length(numlist)) {
    k = numlist[i]
    cat("m =", k, "\n")

    mu_est = mean(c(df[1:k, "Y_time1"], df[1:k, "Y_time2"]))
    mu_est_vec[i] = mu_est
    cat("  mu_est  =", mu_est, "\n")

    rho_est = cor(df[1:k, "Y_time1"], df[1:k, "Y_time2"])
    rho_est_vec[i] = rho_est
    cat("  rho_est =", rho_est, "\n")
  }

  # plots
  par(mfrow=c(1,2))
  plot(x=numlist,
       y=mu_est_vec,
       type="b",
       main=expression(paste(mu[estimate], " by sample size")),
       xlab="sample size",
       ylab=expression(mu[estimate]))
  # add true value
  abline(h=mu, col="red")

  plot(x=numlist,
       y=rho_est_vec,
       type="b",
       main=expression(paste(rho[estimate], " by sample size")),
       xlab="sample size",
       ylab=expression(rho[estimate]))
  # add true value
  abline(h=rho, col="red")
}

assignment2()
