## --------	APPLICATION TO COMPILE CATCH-AT-AGE NUMBERS -------------
## The script starts out by asking for species & area & year to work on.
##
## After this a sampling-summary for the species % area & year is produced.
## This sampling-summary is an overview of the length, weight, age and catch samples
## of the species collected in the specified year and area in a monthly and 
## quarter fashion (saved in file "Yvirlit.txt")
##
## The next step is to calculate the age-length key to get the fleet-based catch-numbers (c@a)
## With the help of the sampling-summary ("Yvirlit.txt") work can now start by typing in
## the fleets and time period to calculate c@a. There is the possibility to borrow samples from
## other more-extensive sampled fleets in cases where sampling in the particular period is sparse,
## poor or non-existent at all. These fleet-based catch-numbers are saved in memory and in data files
## having the following name convention: res_"Species"_"period"_"fleet_number"
##             example 1 -----------> "res_UP_may_aug_19_20_21" <----------
##             example 2 -----------> "res_UP_jan_apr_16_17_18" <----------
##
## Finally the user may opt for combining fleets over periods of time
## A dialog will open in the local directory and the user must choose the files
## to combine (e.g., files 'res_UP_may_aug_19_20_21' and 'res_UP_jan_apr_19_20_21'
##
## To terminate any process or stop altogether just press 'ESC'
##

source('extraFuncs.R', encoding=myencoding)

## ----- READ-IN 'Slag' ----- ##
#slagQuestion <- readline("Sama fiskaslag? (y/n): ")
#if(slagQuestion=='n') 
#{
cat("Fiskaslag", readline("Set inn fiskaslag (up, to, hy ,,,) : "), file="species.in", sep="\n")
Slag <- scan("species.in", skip = 1, quiet= TRUE, what="character")
Slag <- toupper(Slag)
#}

#.- check whether the species is registered or not
while(!(Slag %in% slag.list))
{
cat("\n", Slag,"er ikki skr�set � databasu","\n\n")
cat("Fiskaslag", readline("Set inn fiskaslag (up, to, hy ,,,) : "), file="species.in", sep="\n")
Slag <- scan("species.in", skip = 1, quiet= TRUE, what="character")
Slag <- toupper(Slag)
}

## ----- READ-IN 'Oeki' ----- ##
#areaQuestion <- readline("Sama ICES-�ki? (y/n): ")
#if(areaQuestion=='n') 
#{
cat("ICES-�ki", readline("Set inn ICES-�ki: "), file="driftsOki.in", sep="\n")
Oeki <- scan("driftsOki.in", skip = 1, quiet= TRUE, what="character")
Oeki <- toupper(Oeki)

#.- check whether the ICES-area is correct or not
while(!(all(Oeki %in% c('VB1', 'VB2', 'IIA'))))
{
indexO <- Oeki %in% c('VB1', 'VB2', 'IIA')
cat("\n", Oeki[!indexO],"is not a faroese ICES-area. Re-enter ICES-�ki","\n\n")
cat("ICES-�ki", readline("Set inn ICES-�ki aftur: "), file="driftsOki.in", sep="\n")
Oeki <- scan("driftsOki.in", skip = 1, quiet= TRUE, what="character")
Oeki <- toupper(Oeki)
}

## ----- MAKE sampling-summary ----- ##
summary <- readline("Ynskir t� yvirlitst�l? (y/n): ")
if(summary=='y') 
{
cat("�r fyri yvirlist�l", readline("Set inn �r fyri yvirlitst�l: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)

#.- check whether the year is registered or not
while(!(year_yvirlit %in% year.list))
{
cat("\n", year_yvirlit, "er ikki skr�set � databasu","\n\n")
cat("�r fyri yvirlist�l", readline("Set inn �r fyri yvirlitst�l: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)
}

cat("\n F�lur fyri yvirlitst�l gj�rdur og goymdur her: ",paste(getwd(),'/Yvirlit', "_", Slag, "_", year_yvirlit, ".txt", sep=""),'\n\n')

ArmaS_yvirlit <- as.numeric(paste(substring(year_yvirlit, 3, 4), "00", sep=""))
ArmaE_yvirlit <- as.numeric(paste(substring(year_yvirlit, 3, 4), "13", sep=""))
}

########################################################################################
source('alkFun.R', encoding=myencoding)								 #### -----       ####
source('alkFun_alt.R', encoding=myencoding)								 #### -----       ####
source('veidaFun.R', encoding=myencoding)								 ##### -----     #####
source('aldFun.R', encoding=myencoding)								 ###### -----   ######
source('aldFun_alt.R', encoding=myencoding)								 ###### -----   ######
source('lgdvektFun.R', encoding=myencoding)							 	 ####### ----- #######
source('lgdvektFun_alt.R', encoding=myencoding)							 ####### ----- #######
#source('resFun.R', encoding=myencoding)								 ####### ----- #######
#source('extraFuncs.R', encoding=myencoding)								 ###### -----   ######
source('armaFun_alt.R', encoding=myencoding)								 ###### -----   ######
source('do_data_import.R', encoding=myencoding)							 ##### -----     #####
source('data_import.R', encoding=myencoding)								 #### -----       ####
########################################################################################

#.- make the sequence of year/month 
yymm <- paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep="")

if(summary=='y') 
{
source('yvirlit.R', encoding=myencoding)								 
}

####################			START->SCRIPT TO LOOP 		######################
repeat{ # START repeat  ----------------------------------------------------------------

## ----- READ-IN fleets ----- ##
fleetQuestion <- readline("Sama skipab�lk/-ar? (y/n): ")

if(fleetQuestion=='n') 
{
cat("Skipab�lk/-ar", readline("Set inn skipab�lk/-ar (set millumr�m millum skipabolkanumrini): "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)

#.- check whether the fleets are listed or not in the adopted list "bdrift.lst"
while(!(all(Drift %in% unique(bdrift$drift))))
{
indexF <- Drift %in% unique(bdrift$drift)
cat("\n", Drift[!indexF],"is not listed as a fleet. Re-enter fleets","\n\n")
cat("Skipab�lk/-ar", readline("Set inn skipab�lk/-ar : "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)
}

}

## ----- READ-IN 'ArmaS' & 'ArmaE' ----- ##
cat(paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

cat("Byrjanar og enda �r og m�na� fyri ALK-ALD (YYMM)", readline("Set inn byrjanar �r og m�na� fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")

#.- check whether the Start year&month is correct 
while(!(ArmaS %in% yymm))
{
cat("\n", ArmaS, "is not a proper year&month. Re-enter byrjanar �r og m�na�: ","\n\n")
cat("Byrjanar og enda �r og m�na� fyri ALK-ALD (YYMM)", readline("Set inn byrjanar �r og m�na� fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")
}

cat(paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

ArmaE <- readline("Set inn enda �r og m�na� fyri ALK-ALD (YYMM): ")
ArmaE <- formatC(ArmaE, width=4, flag="0")

#.- check whether the End year&month is correct,  
if(ArmaE %in% yymm)
{
cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
#ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE) 

ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)
ArmaE <- formatC(ArmaE, width=4, flag="0")} else {
while(!(ArmaE %in% yymm))
{
cat("\n", ArmaE, "is not a proper year&month. Re-enter enda �r og m�na�: ")
ArmaE <- readline()
ArmaE <- formatC(ArmaE, width=4, flag="0")
}
cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)
ArmaE <- formatC(ArmaE, width=4, flag="0")
}
#cat(ArmaE, file="driftsArma.in", sep=" ", append=T)
#ArmaE <- scan("driftsArma.in", skip = 2, quiet= TRUE)

## ----- MAKE 'Arma' ----- ##
source('armaFun.R', encoding=myencoding)

source('armaFun_alt.R', encoding=myencoding)


## ----- READ-IN fleets to borrow lengths ----- ##
cat("Skipab�lk/-ar, sum longdir eru l�ntar fr�", readline("Set inn skipab�lk/-ar, sum longdir skulu l�nast fr� (skriva '0' um onki skal l�nast): "), file="driftsLgd.in", sep="\n")
Drift_Lgd_Borrow <- scan("driftsLgd.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow weights ----- ##
cat("Skipab�lk/-ar, sum vektir eru l�ntar fr�", readline("Set inn skipab�lk/-ar, sum vektir skulu l�nast fr� (skriva '0' um onki skal l�nast): "), file="driftsVkt.in", sep="\n")
Drift_Vekt_Borrow <- scan("driftsVkt.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow ages ----- ##
cat("Skipab�lk/-ar, sum aldrar eru l�ntar fr�", readline("Set inn skipab�lk/-ar, sum aldrar skulu l�nast fr�(skriva '0' um onki skal l�nast): "), file="driftsAge.in", sep="\n")
Drift_Age_Borrow <- scan("driftsAge.in", skip = 1, quiet= TRUE)

#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�
#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�

sampleAlt <- readline(paste("Ynskir t� at l�na pr�var fr� einum o�rum �ri enn", year_yvirlit,"? (y/n): "))

if(sampleAlt=='y')	################# Start if(sampleAlt=='y') ################################
{

## ----- READ-IN year & fleets to borrow lengths ----- ##
cat("Skipab�lk/-ar og �rma, sum longdir eru l�ntar fr�", readline("Set inn skipab�lk/-ar, sum longdir skulu l�nast fr� ( '0' um onki skal l�nast): "), file="arma_alt_driftsLgd.in", sep="\n")
Drift_Lgd_Borrow <- scan("arma_alt_driftsLgd.in", skip = 1, quiet= TRUE)
if(any(!Drift_Lgd_Borrow==0))
{
cat(readline("Set inn �r & m�na�, sum longdir skulu l�nast fr� : "), file="arma_alt_driftsLgd.in", sep="\n", append=T)
Arma_alt_Lgd_Borrow    <- scan("arma_alt_driftsLgd.in", skip = 2, quiet= TRUE)

ArmaS_alt_lgd <- Arma_alt_Lgd_Borrow[1]
ArmaE_alt_lgd <- Arma_alt_Lgd_Borrow[2]

armaFun_alt_lgd(ArmaS_alt_lgd, ArmaE_alt_lgd)
}

## ----- READ-IN year & fleets to borrow weights ----- ##
cat("Skipab�lk/-ar og �rma, sum vektir eru l�ntar fr�", readline("Set inn skipab�lk/-ar, sum vektir skulu l�nast fr� ( '0' um onki skal l�nast): "), file="arma_alt_driftsVkt.in", sep="\n")
Drift_Vekt_Borrow <- scan("arma_alt_driftsVkt.in", skip = 1, quiet= TRUE)
if(any(!Drift_Vekt_Borrow==0))
{
cat(readline("Set inn �r & m�na�, sum vektir skulu l�nast fr� : "), file="arma_alt_driftsVkt.in", sep="\n", append=T)
Arma_alt_Vkt_Borrow    <- scan("arma_alt_driftsVkt.in", skip = 2, quiet= TRUE)

ArmaS_alt_vkt <- Arma_alt_Vkt_Borrow[1]
ArmaE_alt_vkt <- Arma_alt_Vkt_Borrow[2]

armaFun_alt_vkt(ArmaS_alt_vkt, ArmaE_alt_vkt)
}

## ----- READ-IN year & fleets to borrow ages ----- ##
cat("Skipab�lk/-ar og �rma, sum aldrar eru l�ntar fr�", readline("Set inn skipab�lk/-ar, sum aldrar skulu l�nast fr� ( '0' um onki skal l�nast): "), file="arma_alt_driftsAge.in", sep="\n")
Drift_Age_Borrow <- scan("arma_alt_driftsAge.in", skip = 1, quiet= TRUE)
if(any(!Drift_Age_Borrow==0))
{
cat(readline("Set inn �r & m�na�, sum aldrar skulu l�nast fr� : "), file="arma_alt_driftsAge.in", sep="\n", append=T)
Arma_alt_Age_Borrow    <- scan("arma_alt_driftsAge.in", skip = 2, quiet= TRUE)

ArmaS_alt_age <- Arma_alt_Age_Borrow[1]
ArmaE_alt_age <- Arma_alt_Age_Borrow[2]

armaFun_alt_age(ArmaS_alt_age, ArmaE_alt_age)
}

## ----- READ-IN the alternative(extra) data from database
source("do_data_import_alt.R", encoding=myencoding)
source("data_import_alt.R", encoding=myencoding)

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
source('resFun_alt.R', echo = TRUE, encoding=myencoding)

source('spool_all_alt.R', echo=T, encoding=myencoding)

## ----- RESTORE data frames alkAll, lgdAll, lgdvektAll
alkAll     <- alkAll_back
lgdAll     <- lgdAll_back
lgdvekt    <- lgdvektAll_back

## ----- PRINT INFORMATION on base-calculations ----- ##

cat("\n","F�lur goymdur sum: ", paste("res_alt", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"),"\n")

cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n")
cat(" > Dato                : ", date(), '\n' 
, "> Slag                : "   , Slag, '\n'
, "> �r                  : "   , year_yvirlit, '\n' 
, "> Ti�arskei�          : "   , gsub("_", "-",toupper(manFun(Arma))), '\n'
, "> Skipab�lk/-ar       : "   , paste(DriftV, collapse=" "), '\n' 
, "> Skipab�lk/-ar, (longd l�n) : "  , Drift_Lgd_Borrow                      , "\n"
, "> Skipab�lk/-ar, (vekt l�n)  : "  , Drift_Vekt_Borrow                     , "\n"
, "> Skipab�lk/-ar, (aldur l�n) : "  , Drift_Age_Borrow                      , "\n"
, "> ICES-�ki            : "   , Oeki, '\n'
, "> -------- Fr� ��rum �ri ---------- "                                     , "\n"
, "> Skipab�lk/-ar (l�n) : "   , lanDrifts(), '\n'
, "> L�ntar pr�var ALK   : "   , sort(unique(ArmaAlk[!(ArmaAlk %in% Arma)])), '\n'
, "> L�ntar pr�var ALD   : "   , sort(unique(ArmaAld[!(ArmaAld %in% Arma)])), '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")

} else {	

#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�
#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�


## ----- PRINT INFORMATION on base-calculations ----- ##

cat("\n","F�lur goymdur sum: ", paste("res", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"),"\n")
cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "^ Slag                                      : "  , Slag                , "\n"
        , "^ Skipab�lk/-ar                             : "  , Drift               , "\n"
        , "^ Byrjanar �r og m�na� fyri ALK/ALD         : "  , ArmaS               , "\n"
        , "^ Enda �r og m�na� fyri ALK/ALD             : "  , ArmaE               , "\n"
        , "^ Skipab�lk/-ar, sum longdir eru l�ntar fr� : "  , Drift_Lgd_Borrow    , "\n"
        , "^ Skipab�lk/-ar, sum vektir eru l�ntar fr�  : "  , Drift_Vekt_Borrow   , "\n"
        , "^ Skipab�lk/-ar, sum aldrar eru l�ntar fr�  : "  , Drift_Age_Borrow    , "\n"
        , "^ ICES-�ki                                  : "  , Oeki                , "\n")
cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

## ----- CALCULATE the fleet-based C@A ----- ##
source('resFun.R', echo = TRUE, encoding=myencoding)

source('spool_all.R', echo=T, encoding=myencoding)

cat("\n\n                     >>>>>>>>>>>>>>>>>>>>>>>>>>                       ")
cat("\n >>>>>>>>>>>>>>>>>>>> Tr�st 'ESC' fyri at enda <<<<<<<<<<<<<<<<<<<< \n")
cat("                     <<<<<<<<<<<<<<<<<<<<<<<<<<                       ")

cat("\n\n         >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ")
cat("\n >>>>>>> Um t� ynskir at ste�ga og halda fram seinni, skriva: source(\"interactive1.R\") <<<<< \n")
cat("         <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< \n\n")


}	################# End if(sampleAlt=='y') ################################

## ----- COMBINATE fleets ----- ##
cat("\n","Vel f�lar sum starta vi� \'res_\', til d�mis \'res_UP_jan_apr_19_20_21\'","\n")

YesCombFlt <- readline("\n Ynskir t� at leggja f�lar saman? (y/n): ")
#Sys.sleep(1)
#cat("\n","Vel f�lar sum starta vi� \'res_\', til d�mis res_UP_jan_apr_19_20_21")

if(YesCombFlt=='y') 
{
res.files.choose  <- choose.files()
source("combine_fleets_interactive.R", echo=T, encoding=myencoding)
}

} # END of repeat ----------------------------------------------------------------------

####################			END->SCRIPT TO LOOP 		######################







