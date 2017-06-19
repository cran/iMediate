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
        mediated = min(l_a, l_b.c)
        mediator = l_a + l_b.c
        total = l_a+l_b.c+l_c
        
        c(l_a, l_b.c, max(l_a, l_b.c), mediated, mediator, l_c, total, mediated/mediator, mediated/total)
    }
    
    obvd = mdn2(fit.M, fit.Y, X, boot=FALSE)
    l_a = obvd[1]
    l_b.c = obvd[2]
    mediator = obvd[5]
    predictor = obvd[6]
    total = obvd[7]

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

    if (B>0){
    	    BS = matrix(0, nrow=B, ncol=7)
    	    for (i in 1:B) BS[i,] = mdn2(fit.M, fit.Y, X)[-(1:2)]
        BS[,1:5] = BS[,1:5]/nn
        lower = apply(BS, 2, quantile, probs = sig.level/2)
        upper = apply(BS, 2, quantile, probs = 1-sig.level/2)

        result = data.frame(Estimate = c(obvd[3:7]/nn, obvd[8:9]),
                            Lower = lower,
                            Upper = upper,
                            Stat = c(obvd[3:7], NA, NA),
                            P = c(NA, 1-Test, 1-pchisq(mediator, 2), 1-pchisq(predictor, 1), 1-pchisq(total, 3), NA, NA))
    }
    else {
        result = data.frame(Estimate = c(obvd[3:7]/nn, obvd[8:9]),
                            Stat = c(obvd[3:7], NA, NA),
                            P = c(NA, 1-Test, 1-pchisq(mediator, 2), 1-pchisq(predictor, 1), 1-pchisq(total, 3), NA, NA))
    }
    dimnames(result)[[1]] = c("  Unmediated", "  Mediated", "Mediator", "Predictor", 
                              "Total", "Mediated/Mediator", "Mediated/Total")
    
    structure(list(result=result, test=test, sig.level=sig.level, sample.size=nn, B=B), class = "mdn")
}

