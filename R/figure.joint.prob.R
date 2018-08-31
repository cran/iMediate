#' Contour Plot of Joint Probabilities related to Mediated Effect
#' 
#' Contour plot of joint probability of mediated effect and total effect in the
#' absence of direct effect and joint probability of mediated effect and direct
#' effect assuming \eqn{ab=c'}
#' 
#' %% ~~ If necessary, more details than the description above ~~ 
#' Basic three-factor mediation model is assumed. Coefficients are standardized such
#' that the variances of treatment, mediator, and outcome are equal to 1. Note
#' that the y-axis is \eqn{a^2} and the x-axis is \eqn{b^2}. The default axes
#' labels from R function \code{plot_ly} are switched in order to make them
#' correct.
#' 
#' @param prob a character string specifying the probability to be plotted. One
#' of \code{"mediated.main"} (default) and \code{"mediated.direct"}.
#' \code{"mediated.main"} requests the probability of the mediated effect and
#' the main effect assuming there is no direct effect (\eqn{c'=0}).
#' \code{"mediated.direct"} requests the probability of the mediated effect and
#' the direct effect assuming \eqn{ab=c'}
#' @param n sample size
#' @param sig.level significance level used for the test of the mediated effect
#' @param grid.size grid size for \eqn{a^2} and \eqn{b^2}
#' @return A plot generated using package \code{plotly}
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @references Wang, K. (2018) Understanding power anomalies in mediation
#' analysis. Psychometrika 83 (2), 387-406.
#' @keywords figure 
#' @examples
#' 
#' # figure.joint.prob()                                # Figure 4 of Wang (2018)
#' # figure.joint.prob(prob="mediated.direct")          # Figure 5 of Wang (2018)
#' 
#' @export figure.joint.prob
#' @importFrom mvtnorm pmvnorm
#' @importFrom plotly plot_ly %>% layout
figure.joint.prob = function(prob = "mediated.main", n=100, sig.level=0.05, grid.size=0.01){
     if(prob != "mediated.main" & prob != "mediated.direct") stop("Unknown probability name")
     
     a2 = seq(0, 1-grid.size, grid.size)
     b2 = seq(0, 1-grid.size, grid.size)
     c.value = qchisq(1-sig.level, 1)

     if (prob == "mediated.main"){
          prob1 = function(a2, b2, n){           
              mu.a2 = a2/(1-a2)
              mu.b2 = b2/((1-b2)/(1-a2))
              mu.c2 = a2*b2/(1-a2*b2)
              cor = sqrt(b2)*sqrt(1-a2)/sqrt(1-a2*b2)
              pi.b = pchisq(c.value, 1, ncp=n*mu.b2, lower.tail=FALSE)
              pi.c = pchisq(c.value, 1, ncp=n*mu.c2, lower.tail=FALSE)
              upper = rep(sqrt(c.value), 2)
              tmp = pmvnorm(lower=-upper, upper=upper, 
                            mean=sqrt(c(n*mu.a2, n*mu.c2)), 
                            corr = rbind(c(1, cor), c(cor, 1)))
              as.vector(pi.b*(1 - pi.c - tmp))
          }
          z = matrix(0, nrow=length(a2), ncol=length(b2))
          for (i in 1:length(a2)){                # outer function does not work with pmvnorm function
	          for (j in 1:length(b2)){
        		  z[i, j] = prob1(a2[i], b2[j], n)
	          }
          }
          plot_ly(x=a2, y=b2, z=z, type="contour") %>%    
              layout(xaxis=list(title="b^2"), yaxis=list(title="a^2"))  
     }
     else {
         prob2 = function(a2, b2, n){
        	if ((1-4*a2*b2)/(1-a2)-b2 > 0){
		        mu.a2 = a2/(1-a2)
                mu.b2 = b2/((1-4*a2*b2)/(1-a2)-b2)
                mu.cp2 = a2*b2/((1-4*a2*b2)/(1-a2)-b2)
                cor = -sqrt(a2)
                pi.a = pchisq(c.value, 1, ncp=n*mu.a2, lower.tail=FALSE)
                pi.cp = pchisq(c.value, 1, ncp=n*mu.cp2, lower.tail=FALSE)
                upper = rep(sqrt(c.value), 2)
                tmp = pmvnorm(lower=-upper, upper=upper, 
                              mean=sqrt(c(n*mu.b2, n*mu.cp2)), 
                              corr = rbind(c(1, cor), c(cor, 1)))
                return(as.vector(pi.a*(1 - pi.cp - tmp)))
            }
            else return(NA)
        }
        z = matrix(NA, nrow=length(a2), ncol=length(b2))
        for (i in 1:length(a2)){                # outer function does not work with pmvnorm function
	        for (j in 1:length(b2)){
		        z[i, j] = prob2(a2[i], b2[j], n)
	        }
        }
        plot_ly(x=a2, y=b2, z=z, type="contour") %>%    
             layout(xaxis=list(title="b^2"), yaxis=list(title="a^2")) 
     }
}

