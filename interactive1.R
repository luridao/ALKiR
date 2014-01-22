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
cat("\n", Slag,"er ikki skráset á databasu","\n\n")
cat("Fiskaslag", readline("Set inn fiskaslag (up, to, hy ,,,) : "), file="species.in", sep="\n")
Slag <- scan("species.in", skip = 1, quiet= TRUE, what="character")
Slag <- toupper(Slag)
}

## ----- READ-IN 'Oeki' ----- ##
#areaQuestion <- readline("Sama ICES-øki? (y/n): ")
#if(areaQuestion=='n') 
#{
cat("ICES-øki", readline("Set inn ICES-øki: "), file="driftsOki.in", sep="\n")
Oeki <- scan("driftsOki.in", skip = 1, quiet= TRUE, what="character")
Oeki <- toupper(Oeki)

#.- check whether the ICES-area is correct or not
while(!(all(Oeki %in% c('VB1', 'VB2', 'IIA'))))
{
indexO <- Oeki %in% c('VB1', 'VB2', 'IIA')
cat("\n", Oeki[!indexO],"is not a faroese ICES-area. Re-enter ICES-øki","\n\n")
cat("ICES-øki", readline("Set inn ICES-øki aftur: "), file="driftsOki.in", sep="\n")
Oeki <- scan("driftsOki.in", skip = 1, quiet= TRUE, what="character")
Oeki <- toupper(Oeki)
}

## ----- MAKE sampling-summary ----- ##
summary <- readline("Ynskir tú yvirlitstøl? (y/n): ")
if(summary=='y') 
{
cat("Ár fyri yvirlistøl", readline("Set inn ár fyri yvirlitstøl: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)

#.- check whether the year is registered or not
while(!(year_yvirlit %in% year.list))
{
cat("\n", year_yvirlit, "er ikki skráset á databasu","\n\n")
cat("Ár fyri yvirlistøl", readline("Set inn ár fyri yvirlitstøl: "), file="driftsArmaSmry.in", sep="\n")
year_yvirlit <- scan("driftsArmaSmry.in", skip = 1, quiet= TRUE)
}

cat("\n Fílur fyri yvirlitstøl gjørdur og goymdur her: ",paste(getwd(),'/Yvirlit', "_", Slag, "_", year_yvirlit, ".txt", sep=""),'\n\n')

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
fleetQuestion <- readline("Sama skipabólk/-ar? (y/n): ")

if(fleetQuestion=='n') 
{
cat("Skipabólk/-ar", readline("Set inn skipabólk/-ar (set millumrúm millum skipabolkanumrini): "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)

#.- check whether the fleets are listed or not in the adopted list "bdrift.lst"
while(!(all(Drift %in% unique(bdrift$drift))))
{
indexF <- Drift %in% unique(bdrift$drift)
cat("\n", Drift[!indexF],"is not listed as a fleet. Re-enter fleets","\n\n")
cat("Skipabólk/-ar", readline("Set inn skipabólk/-ar : "), file="drifts.in", sep="\n")
Drift <- DriftV <- scan("drifts.in", skip = 1, quiet= TRUE)
}

}

## ----- READ-IN 'ArmaS' & 'ArmaE' ----- ##
cat(paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

cat("Byrjanar og enda ár og mánað fyri ALK-ALD (YYMM)", readline("Set inn byrjanar ár og mánað fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")

#.- check whether the Start year&month is correct 
while(!(ArmaS %in% yymm))
{
cat("\n", ArmaS, "is not a proper year&month. Re-enter byrjanar ár og mánað: ","\n\n")
cat("Byrjanar og enda ár og mánað fyri ALK-ALD (YYMM)", readline("Set inn byrjanar ár og mánað fyri ALK-ALD (YYMM): "), file="driftsArma.in", sep="\n")
ArmaS <- scan("driftsArma.in", skip = 1, quiet= TRUE)
ArmaS <- formatC(ArmaS, width=4, flag="0")
}

cat(paste("", substring(months(as.Date(paste("01", 1:12, year_yvirlit, sep="-"))), 1, 3), sep=" ")		# YYMM
, '\n', paste(substring(year_yvirlit, 3, 4), formatC(seq(01, 12), width = 2, flag = "0"), sep=""), '\n')	# example

ArmaE <- readline("Set inn enda ár og mánað fyri ALK-ALD (YYMM): ")
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
cat("\n", ArmaE, "is not a proper year&month. Re-enter enda ár og mánað: ")
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
cat("Skipabólk/-ar, sum longdir eru lántar frá", readline("Set inn skipabólk/-ar, sum longdir skulu lánast frá (skriva '0' um onki skal lánast): "), file="driftsLgd.in", sep="\n")
Drift_Lgd_Borrow <- scan("driftsLgd.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow weights ----- ##
cat("Skipabólk/-ar, sum vektir eru lántar frá", readline("Set inn skipabólk/-ar, sum vektir skulu lánast frá (skriva '0' um onki skal lánast): "), file="driftsVkt.in", sep="\n")
Drift_Vekt_Borrow <- scan("driftsVkt.in", skip = 1, quiet= TRUE)

## ----- READ-IN fleets to borrow ages ----- ##
cat("Skipabólk/-ar, sum aldrar eru lántar frá", readline("Set inn skipabólk/-ar, sum aldrar skulu lánast frá(skriva '0' um onki skal lánast): "), file="driftsAge.in", sep="\n")
Drift_Age_Borrow <- scan("driftsAge.in", skip = 1, quiet= TRUE)

#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤
#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤

sampleAlt <- readline(paste("Ynskir tú at lána prøvar frá einum oðrum ári enn", year_yvirlit,"? (y/n): "))

if(sampleAlt=='y')	################# Start if(sampleAlt=='y') ################################
{

## ----- READ-IN year & fleets to borrow lengths ----- ##
cat("Skipabólk/-ar og Árma, sum longdir eru lántar frá", readline("Set inn skipabólk/-ar, sum longdir skulu lánast frá ( '0' um onki skal lánast): "), file="arma_alt_driftsLgd.in", sep="\n")
Drift_Lgd_Borrow <- scan("arma_alt_driftsLgd.in", skip = 1, quiet= TRUE)
if(any(!Drift_Lgd_Borrow==0))
{
cat(readline("Set inn ár & mánað, sum longdir skulu lánast frá : "), file="arma_alt_driftsLgd.in", sep="\n", append=T)
Arma_alt_Lgd_Borrow    <- scan("arma_alt_driftsLgd.in", skip = 2, quiet= TRUE)

ArmaS_alt_lgd <- Arma_alt_Lgd_Borrow[1]
ArmaE_alt_lgd <- Arma_alt_Lgd_Borrow[2]

armaFun_alt_lgd(ArmaS_alt_lgd, ArmaE_alt_lgd)
}

## ----- READ-IN year & fleets to borrow weights ----- ##
cat("Skipabólk/-ar og Árma, sum vektir eru lántar frá", readline("Set inn skipabólk/-ar, sum vektir skulu lánast frá ( '0' um onki skal lánast): "), file="arma_alt_driftsVkt.in", sep="\n")
Drift_Vekt_Borrow <- scan("arma_alt_driftsVkt.in", skip = 1, quiet= TRUE)
if(any(!Drift_Vekt_Borrow==0))
{
cat(readline("Set inn ár & mánað, sum vektir skulu lánast frá : "), file="arma_alt_driftsVkt.in", sep="\n", append=T)
Arma_alt_Vkt_Borrow    <- scan("arma_alt_driftsVkt.in", skip = 2, quiet= TRUE)

ArmaS_alt_vkt <- Arma_alt_Vkt_Borrow[1]
ArmaE_alt_vkt <- Arma_alt_Vkt_Borrow[2]

armaFun_alt_vkt(ArmaS_alt_vkt, ArmaE_alt_vkt)
}

## ----- READ-IN year & fleets to borrow ages ----- ##
cat("Skipabólk/-ar og Árma, sum aldrar eru lántar frá", readline("Set inn skipabólk/-ar, sum aldrar skulu lánast frá ( '0' um onki skal lánast): "), file="arma_alt_driftsAge.in", sep="\n")
Drift_Age_Borrow <- scan("arma_alt_driftsAge.in", skip = 1, quiet= TRUE)
if(any(!Drift_Age_Borrow==0))
{
cat(readline("Set inn ár & mánað, sum aldrar skulu lánast frá : "), file="arma_alt_driftsAge.in", sep="\n", append=T)
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

cat("\n","Fílur goymdur sum: ", paste("res_alt", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"),"\n")

cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>","\n")
cat(" > Dato                : ", date(), '\n' 
, "> Slag                : "   , Slag, '\n'
, "> Ár                  : "   , year_yvirlit, '\n' 
, "> Tiðarskeið          : "   , gsub("_", "-",toupper(manFun(Arma))), '\n'
, "> Skipabólk/-ar       : "   , paste(DriftV, collapse=" "), '\n' 
, "> Skipabólk/-ar, (longd lán) : "  , Drift_Lgd_Borrow                      , "\n"
, "> Skipabólk/-ar, (vekt lán)  : "  , Drift_Vekt_Borrow                     , "\n"
, "> Skipabólk/-ar, (aldur lán) : "  , Drift_Age_Borrow                      , "\n"
, "> ICES-øki            : "   , Oeki, '\n'
, "> -------- Frá øðrum ári ---------- "                                     , "\n"
, "> Skipabólk/-ar (lán) : "   , lanDrifts(), '\n'
, "> Lántar prøvar ALK   : "   , sort(unique(ArmaAlk[!(ArmaAlk %in% Arma)])), '\n'
, "> Lántar prøvar ALD   : "   , sort(unique(ArmaAld[!(ArmaAld %in% Arma)])), '\n')
cat(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n\n")

} else {	

#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤
#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤#¤


## ----- PRINT INFORMATION on base-calculations ----- ##

cat("\n","Fílur goymdur sum: ", paste("res", Slag, manFun(Arma), paste(DriftV, collapse="_"), sep="_"),"\n")
cat("\n",">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
cat("\n", "^ Slag                                      : "  , Slag                , "\n"
        , "^ Skipabólk/-ar                             : "  , Drift               , "\n"
        , "^ Byrjanar ár og mánað fyri ALK/ALD         : "  , ArmaS               , "\n"
        , "^ Enda ár og mánað fyri ALK/ALD             : "  , ArmaE               , "\n"
        , "^ Skipabólk/-ar, sum longdir eru lántar frá : "  , Drift_Lgd_Borrow    , "\n"
        , "^ Skipabólk/-ar, sum vektir eru lántar frá  : "  , Drift_Vekt_Borrow   , "\n"
        , "^ Skipabólk/-ar, sum aldrar eru lántar frá  : "  , Drift_Age_Borrow    , "\n"
        , "^ ICES-øki                                  : "  , Oeki                , "\n")
cat("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<","\n")

## ----- CALCULATE the fleet-based C@A ----- ##
source('resFun.R', echo = TRUE, encoding=myencoding)

source('spool_all.R', echo=T, encoding=myencoding)

cat("\n\n                     >>>>>>>>>>>>>>>>>>>>>>>>>>                       ")
cat("\n >>>>>>>>>>>>>>>>>>>> Trýst 'ESC' fyri at enda <<<<<<<<<<<<<<<<<<<< \n")
cat("                     <<<<<<<<<<<<<<<<<<<<<<<<<<                       ")

cat("\n\n         >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ")
cat("\n >>>>>>> Um tú ynskir at steðga og halda fram seinni, skriva: source(\"interactive1.R\") <<<<< \n")
cat("         <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< \n\n")


}	################# End if(sampleAlt=='y') ################################

## ----- COMBINATE fleets ----- ##
cat("\n","Vel fílar sum starta við \'res_\', til dømis \'res_UP_jan_apr_19_20_21\'","\n")

YesCombFlt <- readline("\n Ynskir tú at leggja fílar saman? (y/n): ")
#Sys.sleep(1)
#cat("\n","Vel fílar sum starta við \'res_\', til dømis res_UP_jan_apr_19_20_21")

if(YesCombFlt=='y') 
{
res.files.choose  <- choose.files()
source("combine_fleets_interactive.R", echo=T, encoding=myencoding)
}

} # END of repeat ----------------------------------------------------------------------

####################			END->SCRIPT TO LOOP 		######################







