\name{S.test}
\alias{S.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S test of Berger (1996)
}
\description{
\code{S.test} conducts the S test proposed in Berger (1996(
}

\usage{
S.test(u1, u2, alpha)
}

\arguments{
\item{u1 }{
a numerical value between 0 and 1.
}
\item{u2 }{
a numerical value between 0 and 1.
}
\item{alpha }{
a numerical variable specifying the significance level for the test.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%Necessary log-likelihoods are extracted from the two fitted models. Various effects are %then calculated. Significance of the mediated effect is known up to whether it is larger %or smaller than \code{sig.level}. If it is larger, a 1 is reported; otherwise a 0 is %reported. There is no p-value.
%}

\value{
If (u1, u2) falls in the rejection region of the S test, a 1 is returned; otherwise a 0 is returned. 
}

\references{
Berger, R.L. (1997) Likelihood ratio tests and intersection-union tests. Advances in statistical decision theory and applications. Birkh\"{a}user Boston, 225-237.
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
S.test(0.1, 0.4, 0.05)
}

