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