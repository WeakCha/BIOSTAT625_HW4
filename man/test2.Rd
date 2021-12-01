\name{test2}
\alias{test2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Linear Regression
}
\description{
Using Fast matrix computation techniques to do linear regression with given features and target of the dataset.
}
\examples{
data(mtcars)
LinearRegression(mtcars, training_part = 0.8, features = c("mpg","cyl"), target = "qsec", seed = 200)
}
