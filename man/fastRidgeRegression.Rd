\name{fastRidgeRegression}
\alias{fastRidgeRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
a
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
d
}
\usage{
fastRidgeRegression(X_train, Y_train, X_test, Y_test, training_part = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X_train}{
%%     ~~Describe \code{X_train} here~~
}
  \item{Y_train}{
%%     ~~Describe \code{Y_train} here~~
}
  \item{X_test}{
%%     ~~Describe \code{X_test} here~~
}
  \item{Y_test}{
%%     ~~Describe \code{Y_test} here~~
}
  \item{training_part}{
%%     ~~Describe \code{training_part} here~~
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
