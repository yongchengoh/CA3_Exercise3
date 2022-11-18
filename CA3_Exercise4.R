df = with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

nll_lm = function(data, par){
  n = nrow(data)
  # y = data$y
  # x = as.matrix(data[,-1])
  x = model.matrix(~ x1 + x2 + x3, data = df)
  y = df$y
  b = par[1:(length(par)-1)]
  sig = par[length(par)]
  mean = x %*% b
  return(-sum(dnorm(y, mean = mean, sd = sig, log = TRUE)))
}

inits = c(mean(df$y), 0, 0, 0, 2)
fit = optim(par = inits, fn = nll_lm, data = df, hessian=TRUE)
fit$par

# optim looks for parameters which minimize the function, so to maximize the log likelihood, we minimize the negative log likelihood.

x = model.matrix(~ x1 + x2 + x3, data = df)
y = df$y
beta_hat = solve(crossprod(x), crossprod(x, y))
beta_hat

sig_hat = sqrt((crossprod(y) - crossprod(y,x) %*% beta_hat - crossprod(x %*% beta_hat, y) + crossprod(x %*% beta_hat))/(nrow(x) - ncol(x)))
sig_hat

fit2 = lm(y ~ x1 + x2 + x3, df)
fit2$coefficients
sqrt(anova(lm(y ~ x1 + x2 + x3, df))$`Mean Sq`[4])

# part 6 divides by (n - df) instead of n. 