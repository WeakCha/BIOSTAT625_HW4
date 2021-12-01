\name{originalRidgeRegression}
\alias{originalRidgeRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
a
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
f
}
\usage{
originalRidgeRegression(data, features = NULL, target, training_part = 0.8, seed = 200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{features}{
%%     ~~Describe \code{features} here~~
}
  \item{target}{
%%     ~~Describe \code{target} here~~
}
  \item{training_part}{
%%     ~~Describe \code{training_part} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
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
