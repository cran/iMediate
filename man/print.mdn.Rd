\name{print.mdn}
\alias{print.mdn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Method for Class ``mdn''
}
\description{
\code{print.mdn} is the print utility for the output from function \code{mdn}
}

\usage{
\method{print}{mdn}(x, \dots) 
}

\arguments{
  \item{x }{an output from function \code{mdn}}
\item{\dots }{
not used.
}
}


\details{
%%  ~~ If necessary, more details than the description above ~~
The p-value for the mediated effect is displayed as greater or smaller than \code{sig.level}. For instance, if it is not significant at level 0.05, then ``\code{> 0.05}'' is displayed.
}

%\value{
%}


%\references{
%}

\author{
Kai Wang \code{<kai-wang@uiowa.edu>}
}


%\note{
%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%%}

\examples{
data("jobs", package = "mediation")

fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
mdn(fit.M, fit.Y, "treat")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
