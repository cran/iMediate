\name{mdn}
\alias{mdn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mediation Analysis via Likelihood
}
\description{
\code{mdn} conducts mediation analysis in terms of likelihood.
}

\usage{
mdn(fit.M, fit.Y, X, test="LR", sig.level=0.05, B=100)
}

\arguments{
\item{fit.M }{
a fitted model object for mediator. It is an object from which the function \code{logLik} can extract the log-likelihood. Examples include those from ``\code{lm}'', ``\code{glm}'', etc.
}

\item{fit.Y }{
a fitted model object for outcome. It can be of a class different from the model for the mediator 
}
\item{X }{
a character string of the name of the treatment variable.
}
\item{test }{
a character string specifying the test statistic used for the mediated effect. It can be either ``\code{S}'' for the S test proposed in Berger (1996) or ``\code{LR}'' for the LR test discussed in Wang (2017). 
}
\item{sig.level }{
a numerical variable specifying the significance level for the test of the mediated effect.
}
\item{B }{
an integer specifying the number of replicates used for the bootstrapping
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
Necessary log-likelihoods are extracted from the two fitted models. Various effects are then calculated. Significance of the mediated effect is known up to whether it is larger or smaller than \code{sig.level}. If it is larger, a 1 is reported; otherwise a 0 is reported. There is no p-value.
}

\value{
A list with class ``\code{mdn}'' containing the following components:
\item{result}{a data frame containing the results of the mediation analysis. The are five variables. They include estimates of various effects and lower and upper bounds of the boostrap confidence interval at level (1-\code{sig.level}) followed by test statistics and their respective p-values. 
}
\item{test}{a character string specifying the test statistic used for the mediated effect
}
\item{sig.level }{
a numerical variable specifying the significance level for the test of the mediated effect.
}
\item{Sample.size }{
number of subjects in the data
}
\item{B }{
an integer specifying the number of replicates used for the bootstrapping
}
}

\references{
Berger, R.L. (1997) Likelihood ratio tests and intersection-union tests. Advances in statistical decision theory and applications. Birkh\"{a}user Boston, 225-237.

Wang, K. (2017) An approximate uniformly more powerful test of mediated effect. Submitted.

Wang, K. (2017) Mediation analysis via likelihood. Submitted.
}

\author{
Kai Wang \code{<kai-wang@uiowa.edu>}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%%\code{\link{Anscombe.test}} and \code{\link{FT.test}}.
%%}

\examples{
data("jobs", package = "mediation")

fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
mdn(fit.M, fit.Y, "treat")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~mediation }
\keyword{ ~effect size }
\keyword{ ~likelihood }
% __ONLY ONE__ keyword per line
