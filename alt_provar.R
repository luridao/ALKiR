#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�

sampleAlt <- readline(paste("Ynskir t� at l�na pr�var fr� einum o�rum �ri enn", year_yvirlit,"? (y/n): "))

if(sampleAlt=='y')
{

## ----- READ-IN year & fleets to borrow lengths ----- ##
cat("�rma og Skipab�lk/-ar, sum longdir eru l�ntar fr�", readline("Set inn �r & m�na�, sum longdir skulu l�nast fr� : "), file="arma_alt_driftsLgd.in", sep="\n")
Arma_alt_Lgd_Borrow    <- scan("arma_alt_driftsLgd.in", skip = 1, quiet= TRUE)
cat(readline("Set inn skipab�lk/-ar, sum longdir skulu l�nast fr� : "), file="arma_alt_driftsLgd.in", sep="\n", append=T)
Drift_Lgd_Borrow <- scan("arma_alt_driftsLgd.in", skip = 2, quiet= TRUE)

ArmaS_alt_lgd <- Arma_alt_Lgd_Borrow[1]
ArmaE_alt_lgd <- Arma_alt_Lgd_Borrow[2]

armaFun_alt_lgd(ArmaS_alt_lgd, ArmaE_alt_lgd)

## ----- READ-IN year & fleets to borrow weights ----- ##
cat("�rma og Skipab�lk/-ar, sum vektir eru l�ntar fr�", readline("Set inn �r & m�na�, sum vektir skulu l�nast fr� : "), file="arma_alt_driftsVkt.in", sep="\n")
Arma_alt_Vkt_Borrow    <- scan("arma_alt_driftsVkt.in", skip = 1, quiet= TRUE)
cat(readline("Set inn skipab�lk/-ar, sum vektir skulu l�nast fr� : "), file="arma_alt_driftsVkt.in", sep="\n", append=T)
Drift_Vekt_Borrow <- scan("arma_alt_driftsVkt.in", skip = 2, quiet= TRUE)

ArmaS_alt_vkt <- Arma_alt_Vkt_Borrow[1]
ArmaE_alt_vkt <- Arma_alt_Vkt_Borrow[2]

armaFun_alt_vkt(ArmaS_alt_vkt, ArmaE_alt_vkt)

## ----- READ-IN year & fleets to borrow ages ----- ##
cat("�rma og Skipab�lk/-ar, sum aldrar eru l�ntar fr�", readline("Set inn �r & m�na�, sum aldrar skulu l�nast fr� : "), file="arma_alt_driftsAge.in", sep="\n")
Arma_alt_Age_Borrow    <- scan("arma_alt_driftsAge.in", skip = 1, quiet= TRUE)
cat(readline("Set inn skipab�lk/-ar, sum aldrar skulu l�nast fr� : "), file="arma_alt_driftsAge.in", sep="\n", append=T)
Drift_Age_Borrow <- scan("arma_alt_driftsAge.in", skip = 2, quiet= TRUE)

ArmaS_alt_age <- Arma_alt_Age_Borrow[1]
ArmaE_alt_age <- Arma_alt_Age_Borrow[2]

armaFun_alt_age(ArmaS_alt_age, ArmaE_alt_age)

## ----- READ-IN the alternative(extra) data from database
source("do_data_import_alt.R")
source("data_import_alt.R")

## ----- MAKE a back-up of existent data frames alkAll, lgdAll, lgdvektAll and
## ----- MERGE existent data frames (alkAll, lgdAll, lgdvektAll) with the additional ones (alk_alt, lgd_alt, lgdvekt_alt)
alkAll_back     <- alkAll
lgdAll_back     <- lgdAll
lgdvektAll_back <- lgdvektAll
# --------------------------------------
alkAll     <- rbind(alkAll    , alk_alt)
lgdAll     <- rbind(lgdAll    , lgd_alt)
lgdvektAll <- rbind(lgdvektAll, lgdvekt_alt)

## ----- CALCULATE the fleet-based C@A ----- ##
source('resFun_alt.R', echo = TRUE)

source('spool_all_alt.R', echo=T)

## ----- RESTORE a back-up of existent data frames alkAll, lgdAll, lgdvektAll and
alkAll     <- alkAll_back
lgdAll     <- lgdAll_back
lgdvekt    <- lgdvektAll_back


}

#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�
