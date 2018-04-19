generate_data = function(n, p)
{
  cov = matrix(nrow = n, ncol = p)
  response = rnorm(n)
  for (i in 1:n)
  {
    cov[i,] = rnorm(p)
  }
  return(list(covariates = cov, responses = response))
}



model_select = function(covariates, responses, cutoff)
{
  index = c() #storing response column index
  df = data.frame(cov = covariates, res = responses)
  model = lm(formula = res ~ cov.1 + cov.2 + cov.3 + cov.4 + cov.5 + cov.6, data = df)
  summ = model.summary(model)
  for (i in 23:28)
  {
    if (summ[[4]][i] <= cutoff) 
    {
      index = c(index, (i - 22)) #index of the column to keep
    }
  }
  if (length(index) == 0)
  {
    return(index)
  }
  lm_new = lm(formula = L$responses ~ L$covariates[, index])
  mod.sum = model.summary(df_new)
  return(mod.sum$coefficients[,4])
}
