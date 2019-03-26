

#' Likelihood Methods for Statistical Mediation Analysis
#' 
#' \code{iMediate} is a collection of methods developed by our group for
#' mediation analysis. It contains methods built upon likelihoods. Use
#' \code{?iMediate} to see an introduction.
#' 
#' \tabular{ll}{ Package: \tab iMediate\cr Type: \tab Package\cr Version: \tab
#' 0.5\cr Date: \tab 2018-08-29\cr License: \tab GPL (>=2)\cr LazyLoad: \tab
#' yes\cr }
#' 
#' @name iMediate-package
#' @aliases iMediate-package iMediate
#' @docType package
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @references Wang, K. (2018) Understanding power anomalies in mediation analysis. Psychometrika 83 (2), 387-406.
#' @references Wang, K. (2019) Maximum likelihood analysis of mediation models with treatment-mediator interaction. Revision submitted.
#' @references Wang, K. (2019) Likelihood-based analysis of the statistical effects of a treatment on an outcome. To be submitted.
#' @references Berger, R.L. (1997) Likelihood ratio tests and
#' intersection-union tests. Advances in statistical decision theory and
#' applications. Birkh\"auser Boston, 225-237.
#' 
#' @examples
#' data("jobs", package = "mediation")
#' 
#' fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
#' fit.Y <- lm(depress2 ~ treat * job_seek + econ_hard + sex + age, data=jobs)
#' mdn(fit.M, fit.Y, "treat")
#' 
NULL



