generate_data = function(n, p)
{
  cov = matrix(nrow = n, ncol = p)
  response = rnorm(n)
  for (i in 1:n)
  {
    cov[i,] = rnorm(p)
  }
  return(list(mat = cov, responses = response))
}

model_select = function(covariates, responses, cutoff)
{
  index = c()
  for (i in 1:nrow(covariates))
  {
    if(all(covariates[i] <= cutoff))
    {
      index = c(index, i)
    }
  }
  new_cov = covariates[index,]
  new_res = responses[index]
  df = data.frame(cov = new_cov, res = new_res)
  model = lm(formula = res ~ cov, data = df)
  return(model)
}