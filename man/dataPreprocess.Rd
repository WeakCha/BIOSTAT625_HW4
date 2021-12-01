\name{dataPreprocess}
\alias{dataPreprocess}
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
dataPreprocess(data, features, target, training_part = 0.8)
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
