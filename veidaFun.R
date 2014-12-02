# -------- START function to calculate the catch  -------------------------

veidaFun <- function(veidatall, slag, driftv, arma, oeki)
{
driftA <- paste(driftv, collapse="_")	# drifts concatenated

manarQ <- manFun(Arma)			# months in quarter fashion

if(dim(veidaDataAll)[1]!=0)
{
	veidatall <- with(veidatall, veidatall[slag %in% Slag & drift %in% DriftV & arma %in% Arma & oeki %in% Oeki, ])
	veidatall <- sum(veidatall[,'vekt'])*1000
} else veidatall = new_veida_obj

assign(paste("veida", slag, manarQ, driftA, sep="_"), veidatall, env=.GlobalEnv)
return(veidatall)

}
#veidaFun(veidaDataAll, Slag, DriftV, Arma, Oeki)

# -------- END function to calculate the catch  -------------------------
