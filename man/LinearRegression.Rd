\name{LinearRegression}
\alias{LinearRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fast Linear Regression
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function will do a linear regression like a machine learning taste, which means it will split the dataset into training set (with the ratio \code{training_part}) and test set and return the predicted result and the RMSE of the test set.
}
\usage{
LinearRegression(data, training_part = 0.8, features = NULL, target, seed = 200)
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
Li (Richard) Liu
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, training_part = 0.8, features = NULL, target,
    seed = 200)
{
    set.seed(seed)
    res = dataPreprocess(data, features, target, training_part)
    X_train = res$X_train
    X_test = res$X_test
    Y_train = res$Y_train
    Y_test = res$Y_test
    Y_train = as.matrix(Y_train)
    Y_test = as.matrix(Y_test)
    res = fastRidgeRegression(X_train, Y_train, X_test, Y_test,
        training_part)
    beta = res$beta
    pred = res$pred
    RMSE = res$RMSE
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
