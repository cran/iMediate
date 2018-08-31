#' Mediation Analysis via Likelihood
#' 
#' \code{mdn} conducts mediation analysis in terms of likelihood.
#' 
#' %% ~~ If necessary, more details than the description above ~~ 
#' Necessary log-likelihoods are extracted from the two fitted models. Various effects
#' are then calculated. Significance of the mediated effect is known up to
#' whether it is larger or smaller than \code{sig.level}. If it is larger, a 1
#' is reported; otherwise a 0 is reported. There is no p-value.
#' 
#' @param fit.M a fitted model object for mediator. It is an object from which
#' the function \code{logLik} can extract the log-likelihood. Examples include
#' those from ``\code{lm}'', ``\code{glm}'', etc.
#' @param fit.Y a fitted model object for outcome. It can be of a class
#' different from the model for the mediator
#' @param X a character string of the name of the treatment variable.
#' @param test a character string specifying the test statistic used for the
#' mediated effect. It can be either ``\code{S}'' for the S test proposed in
#' Berger (1996) or ``\code{LR}'' for the LR test discussed in Wang (2017).
#' @param sig.level a numerical variable specifying the significance level for
#' the test of the mediated effect.
#' @param B an integer specifying the number of replicates used for the
#' bootstrapping
#' @return A list with class ``\code{mdn}'' containing the following
#' components: \item{result}{a data frame containing the results of the
#' mediation analysis. The are five variables. They include estimates of
#' various effects and lower and upper bounds of the boostrap confidence
#' interval at level (1-\code{sig.level}) followed by test statistics and their
#' respective p-values.  } \item{test}{a character string specifying the test
#' statistic used for the mediated effect } \item{Test}{a numerical value of 0 or 1. 
#' If the specified test statistic is significant, its value is 1; otherwise its value is 0 } \item{sig.level }{ a numerical
#' variable specifying the significance level for the test of the mediated
#' effect. } \item{Sample.size }{ number of subjects in the data } \item{B }{
#' an integer specifying the number of replicates used for the bootstrapping }
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @references Berger, R.L. (1997) Likelihood ratio tests and
#' intersection-union tests. Advances in statistical decision theory and
#' applications. Birkh\"auser Boston, 225-237.
#' 
#' Wang, K. (2017) An approximate uniformly more powerful test of mediated
#' effect. Submitted.
#' 
#' @keywords mediation
#' @examples
#' 
#' data("jobs", package = "mediation")
#' 
#' fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
#' fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
#' mdn(fit.M, fit.Y, "treat")
#' 
#' 
#' @export mdn
mdn = function(fit.M, fit.Y, X, test="LR", sig.level=0.05, B=100) {
    M = all.vars(formula(fit.M))[1]
    nn = length(resid(fit.M))
        
    mdn2 = function(fit.M, fit.Y, X, boot=TRUE){
    	    if(boot) idx = sample(1:nn, replace=TRUE) else idx=1:nn
    	    tmp.M = update(fit.M, data=fit.M$model[idx,])
	    tmp.Y = update(fit.Y, data=fit.Y$model[idx,])
        a0 = update(tmp.M, as.formula(paste(".~. - ", X)))                   # no X effect
        b0 = update(tmp.Y, as.formula(paste(".~. - ", M)))                   # no M effect
        b0c0 = update(tmp.Y, as.formula(paste(".~. - ", M, "-", X)))         # no M and X effect

        l_a = 2*(logLik(tmp.M) - logLik(a0))    
        l_b.c = 2*(logLik(tmp.Y) - logLik(b0))
        l_c = 2*(logLik(b0) - logLik(b0c0)) 
        mediation = min(l_a, l_b.c)
        full = mediation+l_c
        
        c(l_a, l_b.c, l_c, mediation, full, mediation/full)
    }
    
    obvd = mdn2(fit.M, fit.Y, X, boot=FALSE)
    l_a = obvd[1]
    l_b.c = obvd[2]
    sole = obvd[3]
    mediation = obvd[4]
    full = obvd[5]
    prop = obvd[6]

    if(test == "S"){
    	    ra = coef(summary(fit.M))[X, "Estimate"]
	    rb = coef(summary(fit.Y))[M, "Estimate"]
	    Test =  S.test(1-pnorm(sign(ra)*sqrt(l_a)), 1-pnorm(sign(rb)*sqrt(l_b.c)), sig.level) 
    }
    if(test == "LR"){
        c.value = qchisq(1 - sig.level, 1)
	    Test = (l_a >= c.value) & (l_b.c >= c.value)
    }
    if(test != "S" & test != "LR") stop("Unknown test name")

    if (B<10) stop("Not enough permutations")
    BS = matrix(0, nrow=B, ncol=4)
    for (i in 1:B) BS[i,] = mdn2(fit.M, fit.Y, X)[-(1:2)]
    pvalue = c(mean(BS[,1] >= sole), mean(BS[,2] >= mediation), mean(BS[,3] >= full), mean(BS[,4] >= prop))

    BS[,1:3] = BS[,1:3]/nn
    lower = apply(BS, 2, quantile, probs = sig.level/2)
    upper = apply(BS, 2, quantile, probs = 1-sig.level/2)

    result = data.frame(Estimate = c(obvd[3:5]/nn, obvd[6]),
                            Lower = lower, Upper = upper, pvalue = pvalue)
    dimnames(result)[[1]] = c("Sole", "Mediation", "Full", "Prop. of Mediation")

    structure(list(result=result, test=test, Test=Test, sig.level=sig.level, sample.size=nn, B=B), class = "mdn")
}

