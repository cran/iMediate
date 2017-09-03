#' S test of Berger (1996)
#' 
#' \code{S.test} conducts the S test proposed in Berger (1996)
#' 
#' 
#' @param u1 a numerical value between 0 and 1.
#' @param u2 a numerical value between 0 and 1.
#' @param alpha a numerical variable specifying the significance level for the
#' test.
#' @return If (u1, u2) falls in the rejection region of the S test, a 1 is
#' returned; otherwise a 0 is returned.
#' @author Kai Wang \code{<kai-wang@@uiowa.edu>}
#' @references Berger, R.L. (1997) Likelihood ratio tests and
#' intersection-union tests. Advances in statistical decision theory and
#' applications. Birkh\"auser Boston, 225-237.
#' @examples
#' 
#' S.test(0.1, 0.4, 0.05)
#' 
#' @export S.test
S.test = function(u1, u2, alpha){
  	Delta = (abs(2*u1-1) <= 1-alpha) & (abs(2*u2-1) <= 1-alpha)
	S1 = (abs(2*u1-1) > 1-alpha) & (abs(2*u2-1) > 1-alpha)
 	S2 = Delta & (4*abs(u1-u2)<=alpha | 4*abs(u1+u2-1)<=alpha)
 	S3 = Delta & abs(abs(u1-0.5)-abs(u2-0.5)) >= 0.5-3*alpha/4

    any(c(S1, S2, S3))
}
