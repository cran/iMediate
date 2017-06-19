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
