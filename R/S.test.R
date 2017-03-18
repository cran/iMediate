S.test = function(u1, u2, alpha){
  	Delta = (abs(2*u1-1) <= 1-alpha) & (abs(2*u2-1) <= 1-alpha)
	S1 = (abs(2*u1-1) > 1-alpha) & (abs(2*u2-1) > 1-alpha)
 	S2 = Delta & (4*abs(u1-u2)<=alpha | 4*abs(u1+u2-1)<=alpha)
 	S3 = Delta & abs(abs(u1-0.5)-abs(u2-0.5)) >= 0.5-3*alpha/4

    any(c(S1, S2, S3))
}
