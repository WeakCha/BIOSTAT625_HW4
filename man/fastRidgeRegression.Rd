\name{fastRidgeRegression}
\alias{fastRidgeRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fast Ridge Regression with Training and Test Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Fast Ridge Regression with Training and Test Data, it will return the prediction result of the data \code{X_test} and the RMSE between \code{Y_test} and the prediction results.
}
\usage{
fastRidgeRegression(X_train, Y_train, X_test, Y_test, training_part = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{training_part}{
%%     ~~Describe \code{training_part} here~~
\code{training_part} is the ratio for training data.
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
function (X_train, Y_train, X_test, Y_test, training_part = 0.8)
{
    res_svd = svd(X_train)
    tuy = crossprod(res_svd$u, Y_train)
    beta = res_svd$v \%*\% (tuy * res_svd$d/(res_svd$d^2))
    pred = crossprod(t(X_test), beta)
    RMSE = eval_metrics(Y_test, pred)
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
