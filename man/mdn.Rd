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
mdn(fit.M, fit.Y, X, test="S test", sig.level=0.05)
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
a character string specifying the test statistic used for the mediated effect. It can be either ``S test'' for the S test proposed in Berger (1996) or ``LRT'' for the LRT test discussed in Wang (2017). 
}
\item{sig.level }{
a numerical variable specifying the significance level for the test of the mediated effect.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
Necessary log-likelihoods are extracted from the two fitted models. Various effects are then calculated. Significance of the mediated effect is known up to whether it is larger or smaller than \code{sig.level}. If it is larger, a 1 is reported; otherwise a 0 is reported. There is no p-value.
}

\value{
A list with class ``\code{mdn}'' containing the following components:
\item{result}{a data frame containing the results of the mediation analysis. The are two variables. One is estimate of various effects and the other is the p-value
}
\item{test}{a character string specifying the test statistic used for the mediated effect
}
}

\references{
Berger, R.L. (1997) Likelihood ratio tests and intersection-union tests. Advances in statistical decision theory and applications. Birkh\"{a}user Boston, 225-237.

Wang, K. (2016) An approximate uniformly more powerful test of mediated effect. Submitted.

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
library(mediation)

b <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
mdn(b, c, "treat")

## example of obtaining bootstrap confidence interval
B = 100
boot = matrix(0, ncol=B, nrow=7)
for (i in 1:B){
    b.jobs = jobs[sample(1:nrow(jobs), replace=TRUE),]
    b <- lm(job_seek ~ treat + econ_hard + sex + age, data=b.jobs)
    c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=b.jobs)
    boot[,i] = mdn(b, c, "treat")$result[, "Estimate"]
}

apply(boot, 1, quantile, probs=c(0.025, 0.975))


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~mediation }
\keyword{ ~effect size }
\keyword{ ~likelihood }
% __ONLY ONE__ keyword per line
