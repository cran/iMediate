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
#' @param sig.level a numerical variable specifying the significance level for
#' the test of the mediated effect.
#' @param B an integer specifying the number of replicates used in the
#' bootstrapping method for the confidence interval. Default value is 0 and 
#' bootstrapping is not conducted
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
#' Wang, K. (2019) Likelihood-based analysis of the statistical effects of a treatment on an outcome. Submitted.
#' 
#' @keywords mediation
#' @examples
#' data("jobs", package = "mediation")
#' 
#' fit.M <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
#' fit.Y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
#' mdn(fit.M, fit.Y, "treat")
#' mdn(fit.M, fit.Y, "treat", B=100)
#‘ 
#' @export mdn
#' @import MBESS
mdn = function(fit.M, fit.Y, X, sig.level=0.05, B=0) {
	options(warn=-1)

    M = all.vars(formula(fit.M))[1]
    nn = length(resid(fit.M))
        
    mdn2 = function(fit.M, fit.Y, X, idx=1:nn){
    		tmp.M = update(fit.M, data=fit.M$model[idx,])
	    	tmp.Y = update(fit.Y, data=fit.Y$model[idx,])

        a0 = update(tmp.M, as.formula(paste(".~. - ", X)))                   # no X effect
        	b0 = update(tmp.Y, as.formula(paste(".~. - ", M)))                   # no M effect
        	c0 = update(tmp.Y, as.formula(paste(".~. - ", X)))                   # no X effect
        	b0c0 = update(tmp.Y, as.formula(paste(".~. - ", M, "-", X)))         # no M and X effect

        	l_a = 2*(logLik(tmp.M) - logLik(a0))    
        	l_b = 2*(logLik(tmp.Y) - logLik(b0))
        	l_bc0 = 2*(logLik(c0) - logLik(b0c0))
        	l_c = 2*(logLik(tmp.Y) - logLik(c0)) 

        	
       	c(l_a, l_b, l_bc0, l_c)
    }

    LRTs = function(ls) {
    		l_ab = min(ls[1:2])
    		CIE = min(ls[1], ls[3])
    		l_abc = CIE + ls[4]
    		
		c(l_ab, l_abc-l_ab, l_abc, ls[4], CIE)
    }

	NCPs = function(ls) {
		
		ncp = function(stat)
		    ifelse (stat > 10, stat, optimize(function(x) -dchisq(stat, 1, ncp=x), c(0, 200), tol=10^(-6))$minimum)

        ncp_ab = min(ncp(ls[1]), ncp(ls[2]))
        ncp_abc0 = min(ncp(ls[1]), ncp(ls[3]))
        ncp_c = ncp(ls[4])
        ncp_abc = ncp_abc0 + ncp_c

		c(ncp_ab, ncp_abc-ncp_ab, ncp_abc, ncp_c, ncp_abc0, ncp_ab/ncp_abc, ncp_c/ncp_abc)
	}

	CI = function(stat, df=1, sig.level=0.05){
#			if (1-pchisq(stat, df=df) > sig.level/2) lb2 = 0 else lb2 = lochi(stat, df, 1-sig.level)[1]
#			if (pchisq(stat, df=df) < sig.level/2) ub2 = 0 else ub2 = hichi(stat, df, 1-sig.level)[1]
#
#			if (1 - pchisq(stat, df=df) > sig.level) {
#				lb1 = 0 
#				sig = FALSE
#				}
#				else {
#					lb1 = lochi(stat, df, 1-sig.level*2)[1]
#					sig = TRUE
#				}
#		
#		c(lb2, ub2, lb1, sig)
#
            two.sd = conf.limits.nc.chisq(stat, conf.level=1-sig.level, df=df)
            one.sd = conf.limits.nc.chisq(stat, conf.level=1-sig.level*2, df=df)
			sig = (1 - pchisq(stat, df=df) < sig.level)
		
		c(two.sd$Lower.Limit, two.sd$Upper.Limit, one.sd$Lower.Limit, sig)
	}


	CI.iut = function(stat1, stat2, df=1, sig.level=0.05, max.bound=500){
#	    lb.one.sided = function(sig.level=sig.level){
#			if (1-pchisq(stat1, df=df) > sig.level) cc1 = 0 else cc1 = lochi(stat1, df, 1-sig.level*2)[1]
#			if (1-pchisq(stat2, df=df) > sig.level) cc2 = 0 else cc2 = lochi(stat2, df, 1-sig.level*2)[1]
#
#           cc1 = conf.limits.nc.chisq(stat1, conf.level=1-sig.level*2, df=df)$Lower.Limit
#           cc2 = conf.limits.nc.chisq(stat2, conf.level=1-sig.level*2, df=df)$Lower.Limit
#
#		min(cc1, cc2)
#   		}
    
	    lb1 = min(conf.limits.nc.chisq(stat1, alpha.lower=sig.level, alpha.upper=0, conf.level=NULL, df=df)$Lower.Limit,
                  conf.limits.nc.chisq(stat2, alpha.lower=sig.level, alpha.upper=0, conf.level=NULL, df=df)$Lower.Limit)
	    sig = TRUE
    		if (pchisq(stat1, df=df) < 1-sig.level | pchisq(stat2, df=df) < 1-sig.level) {
    			lb1 = 0
    			sig = FALSE
	    		}

        cc1 = conf.limits.nc.chisq(stat1, conf.level=1-sig.level, df=df)
        cc2 = conf.limits.nc.chisq(stat2, conf.level=1-sig.level, df=df)
    		lb2 = min(cc1$Lower.Limit, cc2$Lower.Limit)
		ub2 = max(cc1$Upper.Limit, cc2$Upper.Limit)
    		
#		if (pchisq(stat1, df=df) < sig.level) cc1 = 0 else cc1 = hichi(stat1, df, 1-sig.level*2)[1]
#		if (pchisq(stat2, df=df) < sig.level) cc2 = 0 else cc2 = hichi(stat2, df, 1-sig.level*2)[1]
#   		ub2 = max(cc1, cc2)

		c(lb2, ub2, lb1, sig)
	}

    obvd = mdn2(fit.M, fit.Y, X)
    result = data.frame(LRT = c(LRTs(obvd), NA, NA), NCP = NCPs(obvd))
    CIs = rbind(CI.iut(obvd[1], obvd[2], sig.level=sig.level),
                rep(NA, 4),
				CI.iut(obvd[1]+obvd[4], obvd[3]+obvd[4], df=2, sig.level=sig.level),
				CI(obvd[4], sig.level=sig.level),
				CI.iut(obvd[1], obvd[3], sig.level=sig.level),
				rep(NA, 4),
				rep(NA, 4)
				)
	result = cbind(result, CIs)
    dimnames(result)[[1]] = c("       Indirect", "   Complete Direct", "Total", "       Direct", "   Complete Indirect ", "Prop. IE", "Prop. DE")
    dimnames(result)[[2]] = c("LRT", "NCP", "LB2", "UB2", "LB1", paste("p <", sig.level))


	if(B > 0) {
     	BS = pvalue = matrix(0, nrow=B, ncol=7)
    	    for (i in 1:B){
      			idx = sample(1:nn, replace=TRUE)
    		    tmp = mdn2(fit.M, fit.Y, X, idx=idx)
    		    BS[i,] = NCPs(tmp)
    		    pvalue[i,] = c(pchisq(tmp[1], 1) > 1-sig.level & pchisq(tmp[2], 1) > 1-sig.level,
    		                   NA,
    		                   pchisq(tmp[1]+tmp[4], 2) > 1-sig.level & pchisq(tmp[3]+tmp[4], 2) > 1-sig.level,
				           pchisq(tmp[4], 1) > 1-sig.level,
				       	   pchisq(tmp[1], 1) > 1-sig.level & pchisq(tmp[3], 1) > 1-sig.level,
                           NA, NA)
    	    	}
    	    	
    		lb2 = apply(BS, 2, quantile, probs = sig.level/2)
        ub2 = apply(BS, 2, quantile, probs = 1-sig.level/2)
        lb1 = apply(BS, 2, quantile, probs = sig.level)
     
      	result[,3:5] = cbind(lb2, ub2, lb1)
     	result[,6] = colMeans(pvalue)
     	names(result)[6] = "p value"
}
    	options(warn=0)
    	
    structure(list(result=result, sig.level=sig.level, sample.size=nn, B=B), class = "mdn")
}


