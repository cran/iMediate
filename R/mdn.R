mdn = function(fit.M, fit.Y, X, test="S test", sig.level=0.05) {
    M = all.vars(formula(fit.M))[1]
    a0 = update(fit.M, as.formula(paste(".~. - ", X)))                   # no X effect
    b0 = update(fit.Y, as.formula(paste(".~. - ", M)))                   # no M effect
    b0c0 = update(fit.Y, as.formula(paste(".~. - ", M, "-", X)))         # no M and X effect

    l_a = 2*(logLik(fit.M) - logLik(a0))
    l_b.c = 2*(logLik(fit.Y) - logLik(b0))
    l_c = 2*(logLik(b0) - logLik(b0c0)) 
    mediated = min(l_a, l_b.c)
    mediator = l_a + l_b.c
    total = l_a+l_b.c+l_c

    if(test == "S test"){
    	    ra = coef(summary(fit.M))[X, "Estimate"]
	    rb = coef(summary(fit.Y))[M, "Estimate"]
	    Test =  S.test(1-pnorm(sign(ra)*sqrt(l_a)), 1-pnorm(sign(rb)*sqrt(l_b.c)), sig.level) 
    }
    if(test == "LRT"){
        c.value = qchisq(1 - sig.level, 1)
	    Test = (l_a >= c.value) & (l_b.c >= c.value)
    }
    if(test != "S test" & test != "LRT") stop("Unknown test name")

    result = data.frame(Estimate = c(max(l_a, l_b.c), mediated, mediator, l_c, total,   
                            mediated/mediator, mediated/total),
             P = c(NA, 1-Test, 1-pchisq(mediator, 2), 1-pchisq(l_c, 1), 1-pchisq(total, 3), NA, NA))
    dimnames(result)[[1]] = c("  Unmediated", "  Mediated", "Mediator", "Predictor", 
                              "Overall", "Mediated/Predictor", "Mediated/Overall")
    
    structure(list(result=result, test=test, sig.level=sig.level), class = "mdn")
}

