\name{simp_lin_R}
\alias{simp_lin_R}

\title{
Fitting Simple Linear Regression Models
}
\description{
This function can fit a simple linear regression model and return a list of estimated coefficients, their corresponding standard errors, 95\% confidence intervals, residuals and fitted values.
}
\usage{
simp_lin_R(x, y)
}

\arguments{
  \item{x}{
vector of numbers; predictor
}
  \item{y}{
vector of numbers; outcome
}
}
\details{
Both x and y should be numeric vectors and of the same length.
}
\value{
The function produces a list of vectors and matrices.
}

\author{
Xiangdong Meng
}



\seealso{
A simple linear regression model can also be implemented by the function \code{\link{lm}}.
}
\examples{
x <- rnorm(100)
y <- x + 1 + rnorm(100)
simp_lin_R(x, y)
}
