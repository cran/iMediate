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
#' 
#' data("jobs", package = "mediation")
#' 
#' fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
#' fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
#' mdn(fit.M, fit.Y, "treat")
#' 
#' @export print.mdn
#' @S3method print mdn
print.mdn = function (x, ...) 
{
    cat("\nMediation Analysis via Likelihood \n\n")
    tmp = x$result
    tmp$Estimate = round(tmp$Estimate, 4)
    if (x$B>0){
    	tmp$Lower = round(tmp$Lower, 4)
        tmp$Upper = round(tmp$Upper, 4)
    }
    tmp$Stat = round(tmp$Stat, 4)
    tmp$Stat[is.na(tmp$Stat)] = " "
    tmp$P = signif(tmp$P, 4)
    tmp$P[is.na(tmp$P)] = " "
    if(tmp$P[2]=="1") tmp$P[2] = paste(">", x$sig.level)
    if(tmp$P[2]=="0") tmp$P[2] = paste("<", x$sig.level)
    if (x$B>0){
    	    cf = paste(100*(1-x$sig.level), "%", sep="")
        names(tmp)[2:3] = c(paste("LB", cf, "CI"), paste("UB", cf, "CI"))
    }
    print(tmp)
    cat(paste("\n", x$sample.size, " subjects", sep=""))
    if (x$B>0) cat(paste("\n", x$B, " bootstrap samples for confidence intervals", sep=""))
    cat("\nSignificance of the mediated effect is determined by", x$test, "test")
    cat("\nUpper bound for Mediated/Mediator or Mediated/Total is 0.5\n\n")
}
