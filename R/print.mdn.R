#' Print Method for Class ``mdn''
#' 
#' \code{print.mdn} is the print utility for the output from function
#' \code{mdn}
#' 
#' %% ~~ If necessary, more details than the description above ~~ 
#' The p-value for the mediated effect is displayed as greater or smaller than
#' \code{sig.level}. For instance, if it is not significant at level 0.05, then
#' ``\code{> 0.05}'' is displayed.
#' 
#' @param x an output from function \code{mdn}
#' @param \dots not used.
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @examples
#' data("jobs", package = "mediation")
#' 
#' fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
#' fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
#' mdn(fit.M, fit.Y, "treat")
#' 
#' @export
print.mdn = function (x, ...) 
{
    cat("\nMediation Analysis via Likelihood \n\n")
    tmp = x$result
#    tmp$Estimate = round(tmp$Estimate, 4)
#    	tmp$Lower = round(tmp$Lower, 4)
#    tmp$Upper = round(tmp$Upper, 4)
#    tmp$pvalue = round(tmp$pvalue, 4)
#
#    cf = paste(100*(1-x$sig.level), "%", sep="")
#    names(tmp)[2:3] = c(paste("LB", cf, "CI"), paste("UB", cf, "CI"))
    tmp2 = round(tmp, 4)
    if (names(tmp2)[6] == "p < 0.05"){
    	tmp2[,6] = ifelse(tmp2[,6] < 0.5, FALSE, TRUE)
    }
    tmp2[is.na(tmp2)] = "."

    print(tmp2)
    cat(paste("\nThere are ", x$sample.size, " subjects", sep=""))
    cat(paste("\nThe confidence level is", 1-x$sig.level))
    if (x$B > 0) cat(paste("\n", x$B, " bootstrap samples are used for confidence intervals", sep=""))
    cat("\n\n")
}
