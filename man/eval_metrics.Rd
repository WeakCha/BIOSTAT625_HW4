\name{eval_metrics}
\alias{eval_metrics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Evaluation RMSE for True and Prediction Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compute the RMSE for a given true and prediction data.
}
\usage{
eval_metrics(true, predictions)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true}{
%%     ~~Describe \code{true} here~~
\code{true} is the vector of true values.
}
  \item{predictions}{
%%     ~~Describe \code{predictions} here~~
\code{predictions} is the vector of prediction values.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A scalar RMSE
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
function (true, predictions)
{
    resids = true - predictions
    resids2 = resids^2
    N = length(predictions)
    RMSE = round(sqrt(sum(resids2)/N), 2)
    return(RMSE)
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
