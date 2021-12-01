\name{LinearRegression}
\alias{LinearRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast Linear Regression
}
\description{
Using Fast matrix computation techniques to do linear regression with given features and target of the dataset. Data is splited into training set and test set with the parameter training_part. If \item{training_part} is
}
\usage{

}
\examples{
data(mtcars)
LinearRegression(mtcars, training_part = 0.8, features = c("mpg","cyl"), target = "qsec", seed = 200)
}
