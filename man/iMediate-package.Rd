\name{iMediate-package}
\alias{iMediate-package}
\alias{iMediate}
\docType{package}
\title{
Methods for Statistical Mediation Analysis
}
\description{
\code{iMediate} is a collection of methods developed by our group for mediation analysis. It contains methods built upon likelihoods. Use \code{?iMediate} to see an introduction.
}
\details{
\tabular{ll}{
Package: \tab iMediate\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2017-03-17\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Kai Wang \code{<kai-wang@uiowa.edu>}
}
\references{
Wang, K. (2017) Mediation analysis via likelihood. Submitted.
}

%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
%%\keyword{ ~mediation }
%%\keyword{ ~effect size }
%%\keyword{ ~likelihood }
%\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
data("jobs", package = "mediation")

fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
mdn(fit.M, fit.Y, "treat")
}
