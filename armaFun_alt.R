# -------- START function to generate alternative periods for alk-ald  -----------

armaFun_alt_lgd <- function(armaS, armaE)
{
armaS <- as.integer(armaS)
armaE <- as.integer(armaE)

tmp = seq(armaS, armaE, by=1)
assign("Arma_alt_lgd", tmp, envir = .GlobalEnv)
return(tmp)
}
#armaFun_alt_lgd(ArmaS_alt_lgd, ArmaE_alt_lgd)

# -------- END function to generate alternative periods for alk-ald  -------------


# -------- START function to generate alternative periods for alk-ald  -----------

armaFun_alt_vkt <- function(armaS, armaE)
{
armaS <- as.integer(armaS)
armaE <- as.integer(armaE)

tmp = seq(armaS, armaE, by=1)
assign("Arma_alt_vkt", tmp, envir = .GlobalEnv)
return(tmp)
}
#armaFun_alt_vkt(ArmaS_alt_vkt, ArmaE_alt_vkt)

# -------- END function to generate alternative periods for alk-ald  -------------


# -------- START function to generate alternative periods for alk-ald  -----------

armaFun_alt_age <- function(armaS, armaE)
{
armaS <- as.integer(armaS)
armaE <- as.integer(armaE)

tmp = seq(armaS, armaE, by=1)
assign("Arma_alt_age", tmp, envir = .GlobalEnv)
return(tmp)
}
#armaFun_alt_age(ArmaS_alt_age, ArmaE_alt_age)

# -------- END function to generate alternative periods for alk-ald  -------------

