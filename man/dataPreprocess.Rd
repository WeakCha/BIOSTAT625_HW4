\name{dataPreprocess}
\alias{dataPreprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Data Preprocess for Fast Linear Regression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Data Preprocess for Fast Linear Regression
}
\usage{
dataPreprocess(data, features, target, training_part = 0.8, seed = 200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
\code{data} is a \code{data.frame}.
}
  \item{training_part}{
%%     ~~Describe \code{training_part} here~~
\code{training_part} is the ratio for training data.
}
\item{seed}{
\code{seed} is the random seed, it is used for result reproducibility.
}
  \item{features}{
%%     ~~Describe \code{features} here~~
\code{features} is a vector with the names of the features you want to use as predictors.
}
  \item{target}{
\code{target} is a string with the name of the target. We only allow uni-label predicting in this package.
%%     ~~Describe \code{target} here~~
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  \item{X_train}{
%%     ~~Describe \code{X_train} here~~
The data used for training the linear regression model, with only features.
}
  \item{Y_train}{
%%     ~~Describe \code{Y_train} here~~
The data used for training the linear regression model, with only target.
}
  \item{X_test}{
%%     ~~Describe \code{X_test} here~~
The data used for prediction and computing RMSE, with only features.
}
  \item{Y_test}{
%%     ~~Describe \code{Y_test} here~~
The data used for prediction and computing RMSE, with only target.
}
}
\author{
%%  ~~who you are~~
Li (Richard) Liu
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, features, target, training_part = 0.8)
{
    X = select(data, features)
    Y = select(data, target)
    X = as.matrix(X)
    Y = as.matrix(Y)
    X_row = nrow(X)
    seq_1 = rep(1, X_row)
    X = cbind(seq_1, X)
    index = sample(1:nrow(data), training_part * nrow(data))
    X_train = X[index, ]
    X_test = X[-index, ]
    Y_train = Y[index, ]
    Y_test = Y[-index, ]
    return(list(X_train = X_train, X_test = X_test, Y_train = Y_train,
        Y_test = Y_test))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
