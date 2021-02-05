
simp_lin_R <- function(x,y){
  if(is.numeric(x)&is.numeric(y)&length(x)==length(y))
    output <- simp_lin_cpp(x,y)
  else
    stop("Bad input: x and y should be numeric vectors of the same length!")
  return(output)
}