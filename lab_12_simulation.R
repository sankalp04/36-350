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
  # print("I am here")
  model = lm(formula = res ~ cov.1 + cov.2 + cov.3 + cov.4 + cov.5 + cov.6, data = df)
  summ = summary(model)
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
  mod.sum = summary(lm_new)
  return(mod.sum$coefficients[,4])
}


run_simulation = function(n_trials, n, p, cutoff)
{
  result = c()
  for(i in 1:n_trials)
  {
    data = generate_data(n,p)
    
    cov = data$covariates
    res = data$responses
    
    out = model_select(cov, res, cutoff)
    # out = as.numeric(out)
    result = c(result, out)
  }
  
  return(hist(result))
}

n_val = c(100, 1000,10000)

p_val = c(10,20,50)

for(i in 1:length(p_val))
{
  for(j in 1:length(n_val))
  {
    run_simulation(5, n_val[j], p_val[i], 0.05)
  }
}

