#' Contour Plot of Relative Power: Mediated Effect versus Total Effect,
#' Mediated Effect versus Direct Effect
#' 
#' Contour plot of relative power of mediated effect versus total effect in the
#' absence of direct effect and relative power of mediated effect versus direct
#' effect when \eqn{ab=c'}.
#' 
#' %% ~~ If necessary, more details than the description above ~~ 
#' Basic three-factor mediation model is assumed. Coefficients are standardized such
#' that the variances of treatment, mediator, and outcome are equal to 1. Note
#' that the y-axis is \eqn{a^2} and the x-axis is \eqn{b^2}. The default axes
#' labels from R function \code{plot_ly} are switched in order to make them
#' correct.
#' 
#' @param comparison a character string specifying the relative power to be
#' plotted. One of \code{"mediated2main"} (default) and
#' \code{"mediated2direct"}. \code{"mediated2main"} requests the log of power
#' ratio for the mediated effect versus the main effect assuming there is no
#' direct effect (\eqn{c'=0}). \code{"mediated2direct"} requests the log of
#' power ratio for the mediated effect versus the direct effect assuming
#' \eqn{ab=c'}
#' @param n sample size
#' @param sig.level significance level used for the test of the mediated effect
#' @param grid.size grid size for \eqn{a^2} and \eqn{b^2}
#' @return A plot generated using package \code{plotly}
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @references Wang, K. (2018) Understanding power anomalies in mediation
#' analysis. Psychometrika 83 (2), 387-406.
#' @keywords figure
#' @examples
#' ## figure.relative.power()                                # Figure 2 of Wang (2018)
#' ## figure.relative.power(comparison="mediated2direct")    # Figure 3 of Wang (2018)
#' 
#' @export figure.relative.power
#' @importFrom mvtnorm pmvnorm
#' @importFrom plotly plot_ly %>% layout
figure.relative.power = function(comparison = "mediated2main", n=100, sig.level=0.05, grid.size=0.01){
     if(comparison != "mediated2main" & comparison != "mediated2direct") stop("Unknown comparison")
     
     a2 = seq(0, 1-grid.size, grid.size)
     b2 = seq(0, 1-grid.size, grid.size)
     c.value = qchisq(1-sig.level, 1)

     if (comparison == "mediated2main"){
     	  diff1 = function(a2, b2, n){          # for the case c'=0
	          mu.a2 = a2/(1-a2)
              mu.b2 = b2/((1-b2)/(1-a2))
              mu.c2 = a2*b2/(1-a2*b2)
              pchisq(c.value, 1, ncp=n*mu.a2, lower.tail=FALSE, log.p=TRUE) +  
              pchisq(c.value, 1, ncp=n*mu.b2, lower.tail=FALSE, log.p=TRUE) -
              pchisq(c.value, 1, ncp=n*mu.c2, lower.tail=FALSE, log.p=TRUE)
          }
          plot_ly(x = a2, y=b2, z = outer(a2, b2, FUN="diff1", n), type="contour") %>% 
               layout(xaxis=list(title="b^2"), yaxis=list(title="a^2"))
     }
     else {
         diff2 = function(a2, b2, n){
        	if ((1-4*a2*b2)/(1-a2)-b2 > 0){
               mu.a2 = a2/(1-a2)
               mu.b2 = b2/((1-4*a2*b2)/(1-a2)-b2)
               mu.cp2 = a2*mu.b2
               return(pchisq(c.value, 1, ncp=n*mu.a2, lower.tail=FALSE, log.p=TRUE) +  
                      pchisq(c.value, 1, ncp=n*mu.b2, lower.tail=FALSE, log.p=TRUE) -
                      pchisq(c.value, 1, ncp=n*mu.cp2, lower.tail=FALSE, log.p=TRUE))
            }
            else return(NA)
        }
        z = matrix(NA, nrow=length(a2), ncol=length(b2))
        for (i in 1:length(a2)){                # outer function does not work with pmvnorm function
	        for (j in 1:length(b2)){
		        z[i, j] = diff2(a2[i], b2[j], n)
	        }
        }
        plot_ly(x=a2, y=b2, z=z, type="contour") %>%    
             layout(xaxis=list(title="b^2"), yaxis=list(title="a^2")) 
     }
}
