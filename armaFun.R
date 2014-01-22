# -------- START function to generate periods for alk-ald  -----------

armaFun <- function(armaS, armaE)
{
armaS <- as.integer(armaS)
armaE <- as.integer(armaE)

tmp = seq(armaS, armaE, by=1)
assign("Arma", tmp, envir = .GlobalEnv)
return(tmp)
}
armaFun(ArmaS, ArmaE)

# -------- END function to generate periods for alk-ald  -------------
