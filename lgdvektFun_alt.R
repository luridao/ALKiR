# -------- START function to calculate 'a' and 'b' from lgd-vekt  -----------

lgdvektFun_alt <- function(lgdvektall, slag, drift, drift_lgd_borrow, drift_vekt_borrow, arma, oeki)
{
driftA <- paste(drift, collapse="_")	# drifts concatenated
manarQ <- manFun(Arma)			# months in quarter fashion

# If we are borrowing "lengths", "weights" or both then we need to take into account variables: 'Drift_Lgd_Borrow' and 'Drift_Vekt_Borrow'
# If NOT then just ignore these variables
if(Drift_Lgd_Borrow!=0 || Drift_Vekt_Borrow!=0)
{
lgdvektall <- with(lgdvektall, lgdvektall[slag %in% Slag & drift %in% c(Drift, Drift_Lgd_Borrow, Drift_Vekt_Borrow) & arma %in% arma & oeki %in% Oeki, ])
}
else
lgdvektall <- with(lgdvektall, lgdvektall[slag %in% Slag & drift %in% Drift & arma %in% arma & oeki %in% Oeki, ])

if(dim(lgdvektall)[1]==0) cat(paste("\n Lack of data points to estimate alpha & beta :: Vekt = alpha * Longd ^ beta 
   |
   *--->need to borrow weigth-length pairs from some other fleet \n"))
else
{
# observed length and weigth
lobs = lgdvektall$lgd
wobs = lgdvektall$vekt

lwobs <- data.frame(lgt = lobs, wgt = wobs)

###	Linearization - Estimation	####
lw.logfit = lm(log(wgt) ~ log(lgt), data = lwobs)
a         = exp(lw.logfit$coeff[1])
b         = lw.logfit$coeff[2]
r2        = summary(lw.logfit)$r.squared
N         = summary(lw.logfit)$df[2]+2

# gutted catch 
veida_gutted   <- veidaFun(veidaDataAll, Slag, DriftV, Arma, Oeki)
veida_ungutted <- veida_gutted * 1.11

abr.v     = c(a, b, r2, N, veida_gutted, veida_ungutted)
names(abr.v) <- c('a','b','r2', 'N', 'veida_gutted', 'veida_ungutted')

assign(paste("abr", slag, manarQ, driftA, sep="_"), abr.v, env=.GlobalEnv)
return(abr.v)
}
}

#lgdvektFun_alt(lgdvektAll, Slag, DriftV, Drift_Lgd_Borrow, Drift_Vekt_Borrow, Arma, Oeki)


# -------- END function to calculate 'a' and 'b' from lgd-vekt  -----------