

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
#' @references Wang, K. (2018) Identification and maximum likelihood estimation in mediation models with treatment-mediator interaction and unobserved confounding. Submitted.
#' @examples
#' 
#' data("jobs", package = "mediation")
#' 
#' fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
#' fit.Y <- lm(depress2 ~ treat * job_seek + econ_hard + sex + age, data=jobs)
#' mdn(fit.M, fit.Y, "treat")
#' 
NULL



