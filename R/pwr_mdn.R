#' Power and Sample Size for Mediation Analysis
#' 
#' \code{pwr.mdn} Compute power of tests related to mediation analysis or sample size to achieve desired power.
#' 
#' This model is for the basic three-factor model. If coefficients are standardized, then \eqn{\tau_1=1-a^2} and \eqn{\tau_2=1-(c')^2-b^2-2abc'}.
#' 
#' @param a specified value for coefficient \eqn{a}
#' @param b specified value for coefficient \eqn{b}
#' @param c.p specified value for coefficient \eqn{c'}
#' @param tau1 specified value of the ratio of residual variance of mediator \eqn{M} to the variance of the treatment \eqn{X}
#' @param tau2 specified value of the ratio of residual variance of outcome \eqn{Y} to the variance of the treatment \eqn{X}
#' @param n the sample size available. Either \code{"n"} or \code{"power"} must be provided
#' @param power a value specifying the desired power. Either \code{"n"} or \code{"power"} must be provided
#' @param alpha specified significance level 
#' @return A \eqn{2\times 5} matrix
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @references Wang, K. (2018) Understanding power anomalies in mediation
#' analysis. Psychometrika 83 (2), 387-406.
#' @keywords power
#' @examples
#' n = 100
#' X = rnorm(n)
#' s2X = mean((X-mean(X))^2)
#' a=0.3
#' b=0.3
#' c.p = a*b
#' 
#' pwr.mdn(a, b, c.p, 1/s2X, 1/s2X, alpha=0.05, power=0.8)   
#' pwr.mdn(a, b, c.p, 1/s2X, 1/s2X, alpha=0.05, n=200)
#'
#' ## Using standardized coefficients
#' pwr.mdn(a, b, c.p, 1-a^2, 1-c.p^2-b^2-2*a*b*c.p, alpha=0.05, power=0.8)
#' pwr.mdn(a, b, c.p, 1-a^2, 1-c.p^2-b^2-2*a*b*c.p, alpha=0.05, n=200)
#' 
#' @export pwr.mdn


pwr.mdn = function(a, b, c.p, tau1, tau2, n=NULL, power=NULL, alpha=0.05){
    if (sum(sapply(list(n, power), is.null)) != 1) 
        stop("exactly one of n and power must be NULL")
    if (!is.null(n)){
    	 mu2.a = n*a^2/tau1
	     mu2.b = n*b^2*tau1/tau2
	     mu2.c = n*(a*b+c.p)^2/(b^2*tau1+tau2)
	     mu2.c.p = n*c.p^2/(a^2*tau2/tau1+tau2)
         cvalue = qchisq(1-alpha, 1)
         ppp = c(1-pchisq(cvalue, ncp=mu2.a, 1), 
                 1-pchisq(cvalue, ncp=mu2.b, 1), 
                 1-pchisq(cvalue, ncp=mu2.c, 1), 
                 1-pchisq(cvalue, ncp=mu2.c.p, 1),
                 (1-pchisq(cvalue, ncp=mu2.a, 1))*(1-pchisq(cvalue, ncp=mu2.b, 1)))
         ppp = rbind(ppp, sqrt(c(mu2.a, mu2.b, mu2.c, mu2.c.p, min(mu2.a, mu2.b))))
         colnames(ppp) = c("t_a", "t_b", "t_c", "t_cp", "JST")
         rownames(ppp) = c("Power", "NCP")
        
        cat(paste("Power achieved with sample size", n, "at significance level", alpha, "\n\n"))
        return(ppp)
    }
    if (!is.null(power)){
    	ttt = qnorm(1-alpha/2)+qnorm(power)
        n.a = tau1/a^2*ttt^2
        n.c = (b^2*tau1+tau2)/(a*b+c.p)^2*ttt^2
	    n.b = tau2/(b^2*tau1)*ttt^2
	    n.c.p = (a^2*tau2/tau1+tau2)/c.p^2*ttt^2
    	mu2.a = n.a*a^2/tau1
	    mu2.b = n.b*b^2*tau1/tau2
	    mu2.c = n.c*(a*b+c.p)^2/(b^2*tau1+tau2)
	    mu2.c.p = n.c.p*c.p^2/(a^2*tau2/tau1+tau2)

        nnn = ceiling(c(n.a, n.b, n.c, n.c.p, max(n.a, n.b)))
        nnn = rbind(nnn, sqrt(c(mu2.a, mu2.b, mu2.c, mu2.c.p, min(mu2.a, mu2.b))))
        colnames(nnn) = c("t_a", "t_b", "t_c", "t_cp", "JST")
        rownames(nnn) = c("Power", "NCP")
        cat(paste("Required sample size at significance level", alpha, "to achieve power", power, "\n\n"))
        return(nnn)
    }
}    


