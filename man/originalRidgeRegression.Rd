\name{originalRidgeRegression}
\alias{originalRidgeRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Linaer Regression Algorithm for Comparison
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Linaer Regression Algorithm for Comparison
}
\usage{
originalRidgeRegression(data, features = NULL, target, training_part = 0.8, seed = 200)
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
  \item{features}{
%%     ~~Describe \code{features} here~~
\code{features} is a vector with the names of the features you want to use as predictors.
}
  \item{target}{
\code{target} is a string with the name of the target. We only allow uni-label predicting in this package.
%%     ~~Describe \code{target} here~~
}
  \item{seed}{
  \code{seed} is the seed you want to specify, this ensures the reproducibility of the data.
%%     ~~Describe \code{seed} here~~
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{pred}{The prediction for the test set the algorithm automatically splitted}
\item{RMSE}{The RMSE between the prediction and true target result for the test set}
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
function (data, features = NULL, target, training_part = 0.8,
    seed = 200)
{
    flag = require("glmnet")
    if (flag == FALSE) {
        install.packages("glmnet")
        library(glmnet)
    }
    set.seed(seed)
    index = sample(1:nrow(data), training_part * nrow(data))
    data_train = data[index, ]
    data_test = data[-index, ]
    X_train = data.matrix(data_train[, features])
    Y_train = data_train[, target]
    X_test = data.matrix(data_test[, features])
    Y_test = data_test[, target]
    model = glmnet(X_train, Y_train, alpha = 0, lambda = 0, family = "gaussian")
    pred = predict(model, X_test)
    pred = as.vector(pred)
    RMSE = eval_metrics(Y_test, as.vector(pred))
    pred = as.matrix(pred)
    return(list(pred = pred, RMSE = RMSE))
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
