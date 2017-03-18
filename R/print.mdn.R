print.mdn = function (x, ...) 
{
    cat("\nMediation Analysis via Likelihood \n\n")
    tmp = x$result
    tmp$Estimate = round(tmp$Estimate, 4)
    tmp$P = signif(tmp$P, 4)
    tmp$P[is.na(tmp$P)] = " "
    if(tmp$P[2]=="1") tmp$P[2] = paste(">", x$sig.level)
    if(tmp$P[2]=="0") tmp$P[2] = paste("<", x$sig.level)
    print(tmp)
    cat("\nSignificance of the mediated effect is determined by", x$test)
    cat("\nThe range of Mediated/Mediator or Mediated/Overall is [0, 0.5]")
}
